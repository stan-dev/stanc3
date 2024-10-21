(** Lowering of Stan statements to C++ *)

open Core
open Core.Poly
open Middle
open Cpp
open Lower_expr

let lower_st st adtype =
  lower_unsizedtype_local adtype (SizedType.to_unsized st)

let check_to_string = function
  | Transformation.Lower _ -> Some "greater_or_equal"
  | Upper _ -> Some "less_or_equal"
  | CholeskyCov -> Some "cholesky_factor"
  | LowerUpper _ ->
      Common.ICE.internal_compiler_error
        [%message "LowerUpper is really two other checks tied together"]
  | Offset _ | Multiplier _ | OffsetMultiplier _ ->
      Common.ICE.internal_compiler_error
        [%message "Offset and multiplier don't have a check"]
  | t -> constraint_to_string t

let math_fn_translations = function
  | Internal_fun.FnValidateSize ->
      Some "stan::math::validate_non_negative_index"
  | FnValidateSizeSimplex -> Some "stan::math::validate_positive_index"
  | FnValidateSizeUnitVector -> Some "stan::math::validate_unit_vector_index"
  | FnReadWriteEventsOpenCL x -> Some (x ^ ".wait_for_read_write_events")
  | _ -> None

let trans_math_fn f =
  Option.value ~default:(Internal_fun.to_string f) (math_fn_translations f)

(* Code generation for the right hand side of expressions to initialize objects.
   For scalar types this sets the value to NaN and for containers initializes the memory.
*)
let rec initialize_value st adtype =
  let open Expression_syntax in
  let init_nan =
    (* NB: Never used by TupleAD directly *)
    if adtype = UnsizedType.DataOnly then Exprs.quiet_NaN else Var "DUMMY_VAR__"
  in
  match (adtype, st) with
  | UnsizedType.(DataOnly | AutoDiffable), SizedType.SInt -> Exprs.int_min
  | (DataOnly | AutoDiffable), SReal -> init_nan
  | (DataOnly | AutoDiffable), SComplex ->
      let scalar = local_scalar (SizedType.to_unsized st) adtype in
      Constructor (Types.complex scalar, [init_nan; init_nan])
  | (DataOnly | AutoDiffable), SComplexVector size
   |(DataOnly | AutoDiffable), SComplexRowVector size ->
      let typ = lower_st st adtype in
      typ |::? ("Constant", [lower_expr size; initialize_value SComplex adtype])
  | DataOnly, SVector (_, size)
   |DataOnly, SRowVector (_, size)
   |AutoDiffable, SVector (AoS, size)
   |AutoDiffable, SRowVector (AoS, size) ->
      let typ = lower_st st adtype in
      typ |::? ("Constant", [lower_expr size; init_nan])
  | DataOnly, SMatrix (_, d1, d2) | AutoDiffable, SMatrix (AoS, d1, d2) ->
      let typ = lower_st st adtype in
      typ |::? ("Constant", [lower_expr d1; lower_expr d2; init_nan])
  | (DataOnly | AutoDiffable), SComplexMatrix (d1, d2) ->
      let typ = lower_st st adtype in
      typ
      |::? ( "Constant"
           , [lower_expr d1; lower_expr d2; initialize_value SComplex adtype] )
  | AutoDiffable, SVector (SoA, size) ->
      let typ = lower_possibly_var_decl adtype (SizedType.to_unsized st) SoA in
      Constructor (typ, [initialize_value (SVector (AoS, size)) DataOnly])
  | AutoDiffable, SRowVector (SoA, size) ->
      let typ = lower_possibly_var_decl adtype (SizedType.to_unsized st) SoA in
      Constructor (typ, [initialize_value (SRowVector (AoS, size)) DataOnly])
  | AutoDiffable, SMatrix (SoA, d1, d2) ->
      let typ = lower_possibly_var_decl adtype (SizedType.to_unsized st) SoA in
      Constructor (typ, [initialize_value (SMatrix (AoS, d1, d2)) DataOnly])
  | DataOnly, SArray (t, d) ->
      let typ = lower_st st adtype in
      Constructor (typ, [lower_expr d; initialize_value t adtype])
  | (AutoDiffable | TupleAD _), SArray (t, d) ->
      let typ =
        lower_possibly_var_decl adtype (SizedType.to_unsized st)
          (SizedType.get_mem_pattern t) in
      Constructor (typ, [lower_expr d; initialize_value t adtype])
  | TupleAD ads, STuple subts ->
      let typ = lower_st st adtype in
      InitializerExpr (typ, List.map2_exn ~f:initialize_value subts ads)
  | _, STuple _ | TupleAD _, _ ->
      Common.ICE.internal_compiler_error
        [%message
          "Mismatch between Tuple type and Tuple AD in code gen"
            (st : Expr.Typed.t SizedType.t)
            (adtype : UnsizedType.autodifftype)]

(*Initialize an object of a given size.*)
let lower_assign_sized st adtype (initialize : 'a Stmt.Fixed.Pattern.decl_init)
    =
  match initialize with
  | Assign e -> Some (lower_expr e)
  | Default -> Some (initialize_value st adtype)
  | Uninit -> None

let lower_unsized_decl name ut adtype =
  let type_ =
    match (Transform_Mir.is_opencl_var name, ut) with
    | _, UnsizedType.(UInt | UReal) | false, _ ->
        lower_unsizedtype_local adtype ut
    | true, UArray UInt -> TypeLiteral "matrix_cl<int>"
    | true, _ -> TypeLiteral "matrix_cl<double>" in
  make_variable_defn ~type_ ~name ()

let lower_possibly_opencl_decl name st adtype
    (initialize : 'a Stmt.Fixed.Pattern.decl_init) =
  let ut = SizedType.to_unsized st in
  let mem_pattern = SizedType.get_mem_pattern st in
  match (Transform_Mir.is_opencl_var name, ut) with
  | _, UnsizedType.(UInt | UReal) | false, _ -> (
      match initialize with
      | Assign
          Expr.Fixed.
            { pattern= FunApp (CompilerInternal (Internal_fun.FnReadParam _), _)
            ; _ } ->
          (* Peephole optimization for param reads, avoids copying *)
          Auto
      | _ -> lower_possibly_var_decl adtype ut mem_pattern)
  | true, UArray UInt -> TypeLiteral "matrix_cl<int>"
  | true, _ -> TypeLiteral "matrix_cl<double>"

let lower_sized_decl name st adtype initialize =
  let type_ = lower_possibly_opencl_decl name st adtype initialize in
  let init =
    lower_assign_sized st adtype initialize
    |> Option.value_map ~default:Uninitialized ~f:(fun i -> Assignment i) in
  make_variable_defn ~type_ ~name ~init ()

let lower_decl vident pst adtype initialize =
  match pst with
  | Type.Sized st -> VariableDefn (lower_sized_decl vident st adtype initialize)
  | Unsized ut -> VariableDefn (lower_unsized_decl vident ut adtype)

let lower_profile name body =
  let profile =
    VariableDefn
      (make_variable_defn
         ~type_:(TypeLiteral "stan::math::profile<local_scalar_t__>")
         ~name:"profile__"
         ~init:
           (Construction
              [ Var name
              ; Exprs.templated_fun_call "const_cast"
                  [Ref (TypeLiteral "stan::math::profile_map")]
                  [Var "profiles__"] ])
         ()) in
  Stmts.block (profile :: body)

let lower_bool_expr expr =
  match Expr.Typed.type_of expr with
  | UReal -> Exprs.fun_call "stan::math::as_bool" [lower_expr expr]
  | _ -> lower_expr expr

let rec lower_nonrange_lvalue lvalue =
  match lvalue with
  | lbase, [] -> lower_nonrange_lbase lbase
  | lv, idcs when List.for_all ~f:is_single_index idcs ->
      lower_indexed_simple (lower_nonrange_lbase lv) idcs
  | _, _ ->
      Common.ICE.internal_compiler_error
        [%message "Multi-index must be the last (rightmost) index."]

and lower_nonrange_lbase = function
  | Stmt.Fixed.Pattern.LVariable v -> Var v
  | LTupleProjection (lv, ix) ->
      Exprs.templated_fun_call "std::get"
        [TypeLiteral (string_of_int (ix - 1))]
        [lower_nonrange_lvalue lv]

(* True if expr has a 'shallow' overlap with the lhs, for the purpose of checking if expr needs to be deep copied when it's assigned to the lhs.
   This is 'shallow' in the sense that it doesn't recurse into expressions *)
let expr_overlaps_lhs_ref (lhs_base_ref : 'e Stmt.Fixed.Pattern.lvalue)
    (expr : 'a Expr.Fixed.t) : bool =
  Option.value_map
    (* Convert the expression to an lvalue to get rid of everything but variables and indices *)
    (Stmt.Helpers.lvalue_of_expr_opt expr)
    (* If we can't, this expression can't be deep copied *)
    ~default:false
      (* If we can, then find it's base reference and see if it overlaps with the LHS *)
    ~f:(fun expr_lv ->
      let expr_base_ref = Stmt.Helpers.lvalue_base_reference expr_lv in
      expr_base_ref = lhs_base_ref)

let throw_exn exn_type args =
  let open Expression_syntax in
  let err_strm_name = "errmsg_stream__" in
  let stream_decl =
    VariableDefn
      (make_variable_defn ~type_:(TypeLiteral "std::stringstream")
         ~name:err_strm_name ()) in
  let throw = Throw (Exprs.fun_call exn_type [(Var err_strm_name).@!("str")]) in
  let add_to_string e =
    Expression
      (fun_call "stan::math::stan_print" [VarRef err_strm_name; lower_expr e])
  in
  Stmts.block ((stream_decl :: List.map ~f:add_to_string args) @ [throw])

let rec lower_statement Stmt.Fixed.{pattern; meta} : stmt list =
  let remove_promotions (e : 'a Expr.Fixed.t) =
    (* assignment handles one level of promotion internally, don't do it twice *)
    match e.pattern with
    | Promotion (_, UTuple _, _) -> e
    | Promotion (e, _, _) -> e
    | _ -> e in
  let location =
    match pattern with
    | Block _ | SList _
     |Decl {initialize= Default | Uninit; _}
     |Skip | Break | Continue ->
        []
    | _ -> Numbering.assign_loc meta in
  let wrap_e e = [Expression e] in
  let open Expression_syntax in
  location
  @
  match pattern with
  | Assignment (((_, []) as l), _, e)
    when Expr.Typed.compare (Stmt.Helpers.expr_of_lvalue l ~meta:e.meta) e = 0
    ->
      (* self-assign is a no-op *)
      [ Comment
          ("self-assignment omitted: " ^ (Fmt.to_to_string Expr.Typed.pp) e) ]
  | Assignment
      ( (((LVariable _ | LTupleProjection _) as lhs), [])
      , _
      , (( {meta= {Expr.Typed.Meta.type_= UInt | UReal | UComplex; _}; _}
         | { pattern= FunApp (CompilerInternal (FnReadData | FnReadParam _), _)
           ; _ } ) as rhs) ) ->
      Assign (lower_nonrange_lbase lhs, lower_expr rhs) |> wrap_e
  | Assignment ((LVariable assignee, idcs), (UInt | UReal | UComplex), rhs)
    when List.for_all ~f:is_single_index idcs ->
      Assign (lower_indexed_simple (Var assignee) idcs, lower_expr rhs)
      |> wrap_e
  | Assignment (lhs, _, rhs) ->
      (* Assignments of arrays, vectors etc. need to use `assign()` and worry about deep copies *)
      (* XXX I think in general we don't need to do a deepcopy if e is nested
         inside some function call - the function should get its own copy
         (in all cases???) *)
      let lhs_ref = Stmt.Helpers.lvalue_base_reference lhs in
      let rec maybe_deep_copy e =
        match e.Expr.Fixed.pattern with
        (* Never need to copy a scalar type *)
        | _ when UnsizedType.is_scalar_type (Expr.Typed.type_of e) -> e
        (* Never need to copy exprs inside a compiler FunApp *)
        | FunApp (CompilerInternal _, _) -> e
        (* When the expression overlaps with the LHS, *)
        | _ when expr_overlaps_lhs_ref lhs_ref e ->
            (* then wrap it in a deep copy. *)
            { e with
              Expr.Fixed.pattern= FunApp (CompilerInternal FnDeepCopy, [e]) }
        | _ ->
            (* Otherwise recurse on subexpressions *)
            { e with
              Expr.Fixed.pattern=
                Expr.Fixed.Pattern.map maybe_deep_copy e.pattern } in
      let rhs = maybe_deep_copy (remove_promotions rhs) in
      (* Split up the top-level lvalue to fit in the assign call *)
      let lhs_base, lhs_idcs = lhs in
      Exprs.fun_call "stan::model::assign"
        ([ lower_nonrange_lbase lhs_base; lower_expr rhs
         ; Exprs.literal_string
             ("assigning variable " ^ Stmt.Helpers.get_lhs_name lhs) ]
        @ List.map ~f:lower_index lhs_idcs)
      |> wrap_e
  | TargetPE e ->
      let accum = Var "lp_accum__" in
      accum.@?("add", [lower_expr e]) |> wrap_e
  | JacobianPE e ->
      let accum = Var "lp_accum__" in
      [ Stmts.if_block (Var "jacobian__")
          (accum.@?("add", [lower_expr e]) |> wrap_e) ]
  | NRFunApp (CompilerInternal FnPrint, args) ->
      let open Expression_syntax in
      let pstream = Var "pstream__" in
      let print a =
        Expression
          (Exprs.fun_call "stan::math::stan_print" [pstream; lower_expr a])
      in
      let body =
        List.map ~f:print args
        @ [Expression (Deref pstream << [Cpp.Literal "std::endl"])] in
      [Stmts.if_block pstream body]
  | NRFunApp (CompilerInternal FnReject, args) ->
      [throw_exn "std::domain_error" args]
  | NRFunApp (CompilerInternal FnFatalError, args) ->
      [throw_exn "std::runtime_error" args]
  | NRFunApp (CompilerInternal (FnCheck {trans; var_name; var}), args) ->
      Option.value_map (check_to_string trans) ~default:[] ~f:(fun check_name ->
          let function_arg = Expr.Helpers.variable "function__" in
          Exprs.fun_call
            ("stan::math::check_" ^ check_name)
            ([ lower_expr function_arg; Exprs.literal_string var_name
             ; lower_expr var ]
            @ List.map ~f:lower_expr args)
          |> wrap_e)
  | NRFunApp (CompilerInternal (FnWriteParam {unconstrain_opt; var}), _) -> (
      let out = Var "out__" in
      match
        (unconstrain_opt, Option.bind ~f:constraint_to_string unconstrain_opt)
      with
      (* When the current block or this transformation doesn't require unconstraining,
         use vanilla write *)
      | None, _ | _, None -> out.@?("write", [lower_expr var]) |> wrap_e
      (* Otherwise, use stan::io::serializer's write_free functions *)
      | Some trans, Some unconstrain_string ->
          let unconstrain_args = transform_args trans in
          let write_fn = "write_free_" ^ unconstrain_string in
          out.@?(write_fn, lower_exprs (unconstrain_args @ [var])) |> wrap_e)
  | NRFunApp (CompilerInternal f, args) ->
      let fname = trans_math_fn f in
      Exprs.fun_call fname (lower_exprs args) |> wrap_e
  | NRFunApp (StanLib (fname, _, _), args) ->
      Exprs.fun_call (stan_namespace_qualify fname) (lower_exprs args) |> wrap_e
  | NRFunApp (UserDefined (fname, suffix), args) ->
      lower_user_defined_fun fname suffix args |> wrap_e
  | Skip -> []
  | IfElse (cond, ifbranch, elsebranch) ->
      [ IfElse
          ( lower_bool_expr cond
          , Stmts.block (lower_statement ifbranch)
          , Option.map ~f:(Fn.compose Stmts.block lower_statement) elsebranch )
      ]
  | While (cond, body) ->
      [While (lower_bool_expr cond, Stmts.block (lower_statement body))]
  | For {loopvar; lower; upper; body} ->
      [ Stmts.fori loopvar (lower_expr lower) (lower_expr upper)
          (Stmts.block (lower_statement body)) ]
  | Break -> [Break]
  | Continue -> [Continue]
  | Return e -> [Return (Option.map ~f:lower_expr e)]
  | Block ls -> [Stmts.block (lower_statements ls)]
  | SList ls -> lower_statements ls
  | Decl {decl_adtype; decl_id; decl_type; initialize} ->
      [lower_decl decl_id decl_type decl_adtype initialize]
  | Profile (name, ls) -> [lower_profile name (lower_statements ls)]

and lower_statements = List.concat_map ~f:lower_statement

module Testing = struct
  let%expect_test "set size mat array" =
    let int = Expr.Helpers.int in
    Fmt.str "@[<v>%a@]"
      (Fmt.option Cpp.Printing.pp_expr)
      (lower_assign_sized
         (SArray (SArray (SMatrix (AoS, int 2, int 3), int 4), int 5))
         DataOnly Stmt.Fixed.Pattern.Uninit)
    |> print_endline;
    [%expect {| |}]

  let%expect_test "set size mat array" =
    let int = Expr.Helpers.int in
    Fmt.str "@[<v>%a@]"
      (Fmt.option Cpp.Printing.pp_expr)
      (lower_assign_sized
         (SArray (SArray (SMatrix (AoS, int 2, int 3), int 4), int 5))
         DataOnly Stmt.Fixed.Pattern.Default)
    |> print_endline;
    [%expect
      {|
    std::vector<std::vector<Eigen::Matrix<double,-1,-1>>>(5,
      std::vector<Eigen::Matrix<double,-1,-1>>(4,
        Eigen::Matrix<double,-1,-1>::Constant(2, 3,
          std::numeric_limits<double>::quiet_NaN()))) |}]
end
