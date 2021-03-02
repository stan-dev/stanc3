open Core_kernel
open Middle
open Fmt
open Expression_gen

let pp_call_str ppf (name, args) = pp_call ppf (name, string, args)
let pp_block ppf (pp_body, body) = pf ppf "{@;<1 2>@[<v>%a@]@,}" pp_body body

let pp_profile ppf (pp_body, name, body) =
  let profile =
    Fmt.strf
      "profile<local_scalar_t__> profile__(%s, \
       const_cast<profile_map&>(profiles__));"
      name
  in
  pf ppf "{@;<1 2>@[<v>%s@;@;%a@]@,}" profile pp_body body

let rec contains_eigen = function
  | UnsizedType.UTuple ts -> List.exists ~f:contains_eigen ts
  | UnsizedType.UArray t -> contains_eigen t
  | UMatrix | URowVector | UVector -> true
  | UInt | UReal | UMathLibraryFunction | UFun _ -> false

let pp_set_size ppf (decl_id, st, adtype) =
  (* TODO: generate optimal adtypes for expressions and declarations *)
  let real_nan =
    match adtype with
    | UnsizedType.AutoDiffable -> "DUMMY_VAR__"
    | DataOnly -> "std::numeric_limits<double>::quiet_NaN()"
    | TupleAD _ -> raise_s [%message "pp_set_size TUPLES STUB"]
  in
  let rec pp_size_ctor ppf st =
    let pp_st ppf st =
      pf ppf "%a" pp_unsizedtype_local (adtype, SizedType.to_unsized st)
    in
    match st with
    | SizedType.SInt -> pf ppf "std::numeric_limits<int>::min()"
    | SReal -> pf ppf "%s" real_nan
    | SVector d | SRowVector d -> pf ppf "%a(%a)" pp_st st pp_expr d
    | SMatrix (d1, d2) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d1 pp_expr d2
    | SArray (t, d) -> pf ppf "%a(%a, %a)" pp_st st pp_expr d pp_size_ctor t
    (* TUPLE MAYBE c++ initialization

       This is setting the initial value, which is filling the size if eigen
       Should be recursive

       From https://en.cppreference.com/w/cpp/utility/tuple:
       std::tuple<type,..>{value,..};
      *)
    | STuple ts as t ->
        pf ppf "%a{%a}" pp_st t (list ~sep:(Fmt.unit ", ") pp_size_ctor) ts
  in
  pf ppf "@[<hov 2>%s = %a;@]@," decl_id pp_size_ctor st ;
  if contains_eigen (SizedType.to_unsized st) then
    pf ppf "@[<hov 2>stan::math::fill(%s, %s);@]@," decl_id real_nan

let%expect_test "set size mat array" =
  let int = Expr.Helpers.int in
  strf "@[<v>%a@]" pp_set_size
    ("d", SArray (SArray (SMatrix (int 2, int 3), int 4), int 5), DataOnly)
  |> print_endline ;
  [%expect
    {|
      d = std::vector<std::vector<Eigen::Matrix<double, -1, -1>>>(5, std::vector<Eigen::Matrix<double, -1, -1>>(4, Eigen::Matrix<double, -1, -1>(2, 3)));
      stan::math::fill(d, std::numeric_limits<double>::quiet_NaN()); |}]

(** [pp_for_loop ppf (loopvar, lower, upper, pp_body, body)] tries to
    pretty print a for-loop from lower to upper given some loopvar.*)
let pp_for_loop ppf (loopvar, lower, upper, pp_body, body) =
  pf ppf "@[<hov>for (@[<hov>int %s = %a;@ %s <= %a;@ ++%s@])" loopvar pp_expr
    lower loopvar pp_expr upper loopvar ;
  pf ppf " %a@]" pp_body body

let rec integer_el_type = function
  | SizedType.SReal | SVector _ | SMatrix _ | SRowVector _ | STuple _ -> false
  | SInt -> true
  | SArray (st, _) -> integer_el_type st

let pp_decl ppf (vident, ut, adtype) =
  let pp_type =
    match (Transform_Mir.is_opencl_var vident, ut) with
    | _, UnsizedType.(UInt | UReal) | false, _ -> pp_unsizedtype_local
    | true, UArray UInt -> fun ppf _ -> pf ppf "matrix_cl<int>"
    | true, _ -> fun ppf _ -> pf ppf "matrix_cl<double>"
  in
  pf ppf "%a %s;" pp_type (adtype, ut) vident

let pp_sized_decl ppf (vident, st, adtype) =
  pf ppf "%a@,%a" pp_decl
    (vident, SizedType.to_unsized st, adtype)
    pp_set_size (vident, st, adtype)

let pp_possibly_sized_decl ppf (vident, pst, adtype) =
  match pst with
  | Type.Sized st -> pp_sized_decl ppf (vident, st, adtype)
  | Unsized ut -> pp_decl ppf (vident, ut, adtype)

let math_fn_translations = function
  | Internal_fun.FnLength -> Some ("length", [])
  | FnValidateSize -> Some ("validate_non_negative_index", [])
  | FnValidateSizeSimplex -> Some ("validate_positive_index", [])
  | FnValidateSizeUnitVector -> Some ("validate_unit_vector_index", [])
  | _ -> None

let trans_math_fn fname =
  Option.(
    value ~default:(fname, [])
      (bind (Internal_fun.of_string_opt fname) ~f:math_fn_translations))

let pp_bool_expr ppf expr =
  match Expr.Typed.type_of expr with
  | UReal -> pp_call ppf ("as_bool", pp_expr, [expr])
  | _ -> pp_expr ppf expr

let rec pp_nonrange_lvalue ppf lvalue =
  match lvalue with
  | Stmt.Fixed.Pattern.LVariable v -> Fmt.string ppf v
  | LIndexedTuple (lv, ix) ->
      Fmt.pf ppf "std::get<%d>(%a)" ix pp_nonrange_lvalue lv
  | LIndexed (lv, idcs) when List.for_all ~f:is_single_index idcs ->
      pf ppf "%a%a" pp_nonrange_lvalue lv pp_indices_simple ("", idcs)
  | LIndexed (_, _) ->
      (* TODO TUPLE catch multi-index in semantic check, maybe it's already a type error? *)
      raise_s [%message "Multi-index must be the last (rightmost) index."]

(* True if expr has a 'shallow' overlap with the lhs, for the purpose of checking if expr needs to be deep copied when it's assigned to the lhs.
This is 'shallow' in the sense that it doesn't recurse into expressions *)
let expr_needs_deep_copy (lhs_base_ref : 'e Middle.Stmt.Fixed.Pattern.lvalue)
    (expr : 'a Expr.Fixed.t) : bool =
  Option.value_map
    (* Convert the expression to an lvalue to get rid of everything but variables and indices *)
    (Middle.Utils.lvalue_of_expr_opt expr)
    (* If we can't, this expression can't be deep copied *)
    ~default:
      false
      (* If we can, then find it's base reference and see if it overlaps with the LHS *)
    ~f:(fun expr_lv ->
      let expr_base_ref = Middle.Utils.lvalue_base_reference expr_lv in
      expr_base_ref = lhs_base_ref )

let rec pp_statement (ppf : Format.formatter)
    (Stmt.Fixed.({pattern; meta}) as stmt) =
  (* ({stmt; smeta} : (mtype_loc_ad, 'a) stmt_with) = *)
  let pp_stmt_list = list ~sep:cut pp_statement in
  ( match pattern with
  | Block _ | SList _ | Decl _ | Skip | Break | Continue -> ()
  | _ -> Locations.pp_smeta ppf meta ) ;
  match pattern with
  (* TUPLE MAYBE assigning to tuples
     Right now, tuples are assigned with `=`.
     There's a potential issue when the tuple has an element that would normally be assigned to with `assign()`. I'm not sure what sort of checks are done inside `assign`, but they currently aren't being called on e.g. matrices inside tuples.
  *)
  (* Use = for any tuple-valued assignment or any scalar-valued assignment without LHS indices *)
  | Assignment
      ( lhs , _ , ({meta= {Expr.Typed.Meta.type_= UTuple _; _}; _} as rhs))
  | Assignment
      ( ((LVariable _ | LIndexedTuple _ | LIndexed (_, [])) as lhs)
      , _
      , ({meta= {Expr.Typed.Meta.type_= UInt | UReal ; _}; _} as rhs)
      ) ->
      pf ppf "@[<hov 4>%a = %a;@]" pp_nonrange_lvalue lhs pp_expr rhs
  | Assignment (lhs, _, rhs) ->
      (* Assignments of arrays, vectors etc. need to use `assign()` and worry about deep copies *)
      let lhs_ref = Middle.Utils.lvalue_base_reference lhs in
      let rec maybe_deep_copy e =
        (* If this expression overlaps with lhs, *)
        if expr_needs_deep_copy lhs_ref e then
          (* then wrap it in a deep copy. *)
          { e with
            Expr.Fixed.pattern=
              FunApp (CompilerInternal, "stan::model::deep_copy", [e]) }
        else
          (* Otherwise, check if any subexpressions need copies. *)
          match e.pattern with
          (* Don't recurse into function applications because they always copy anyway *)
          | FunApp _ -> e
          | _ ->
              (* Recurse on subexpressions *)
              { e with
                Expr.Fixed.pattern=
                  Expr.Fixed.Pattern.map maybe_deep_copy e.pattern }
      in
      let rhs =
        match rhs.pattern with
        | FunApp (CompilerInternal, f, _)
          when f = Internal_fun.to_string FnConstrain
               || f = Internal_fun.to_string FnUnconstrain ->
            rhs
        | _ -> maybe_deep_copy rhs
      in
      (* Split up the top-level lvalue to fit in the assign call *)
      let lhs_base, lhs_idcs =
        match lhs with LIndexed (lv, idcs) -> (lv, idcs) | _ -> (lhs, [])
      in
      pf ppf "@[<hov 2>assign(@,%a,@ %a,@ %a,@ %S@]);" pp_nonrange_lvalue
        lhs_base pp_indexes lhs_idcs pp_expr rhs
        (strf "assigning variable %a" pp_nonrange_lvalue
           lhs_base
           (* (list ~sep:comma (Pretty.pp_index Pretty.pp_expr_typed_located)) idcs *))
  | TargetPE e -> pf ppf "@[<hov 2>lp_accum__.add(@,%a@]);" pp_expr e
  | NRFunApp (CompilerInternal, fname, args)
    when fname = Internal_fun.to_string FnPrint ->
      let pp_arg ppf a = pf ppf "stan_print(pstream__, %a);" pp_expr a in
      let args = args @ [Expr.Helpers.str "\n"] in
      pf ppf "if (pstream__) %a" pp_block (list ~sep:cut pp_arg, args)
  | NRFunApp (CompilerInternal, fname, args)
    when fname = Internal_fun.to_string FnReject ->
      let err_strm = "errmsg_stream__" in
      let add_to_string ppf e = pf ppf "%s << %a;" err_strm pp_expr e in
      pf ppf "std::stringstream %s;@," err_strm ;
      pf ppf "%a@," (list ~sep:cut add_to_string) args ;
      pf ppf "throw std::domain_error(%s.str());" err_strm
  | NRFunApp
      (CompilerInternal, fname, {pattern= Lit (Str, check_name); _} :: args)
    when fname = Internal_fun.to_string FnCheck ->
      let args =
        {Expr.Fixed.pattern= Var "function__"; meta= Expr.Typed.Meta.empty}
        :: args
      in
      pp_statement ppf
        { pattern= NRFunApp (CompilerInternal, "check_" ^ check_name, args)
        ; meta= stmt.meta }
  | NRFunApp (CompilerInternal, fname, [var])
    when fname = Internal_fun.to_string FnWriteParam ->
      pf ppf "@[<hov 2>vars__.emplace_back(@,%a);@]" pp_expr var
  | NRFunApp (CompilerInternal, fname, args) ->
      let fname, extra_args = trans_math_fn fname in
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
        (extra_args @ args)
  | NRFunApp (StanLib, fname, args) ->
      pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr) args
  | NRFunApp (UserDefined, fname, args) ->
      pf ppf "%a;" pp_user_defined_fun (fname, args)
  | Break -> string ppf "break;"
  | Continue -> string ppf "continue;"
  | Return e -> pf ppf "@[<hov 4>return %a;@]" (option pp_expr) e
  | Skip -> string ppf ";"
  | IfElse (cond, ifbranch, elsebranch) ->
      let pp_else ppf x = pf ppf "else %a" pp_statement x in
      pf ppf "if (@[<hov>%a@]) %a %a" pp_bool_expr cond pp_block_s ifbranch
        (option pp_else) elsebranch
  | While (cond, body) ->
      pf ppf "while (@[<hov>%a@]) %a" pp_bool_expr cond pp_block_s body
  | For
      { body=
          { pattern=
              Assignment (_, _, {pattern= FunApp (CompilerInternal, f, _); _}); _
          } as body; _ }
    when Internal_fun.of_string_opt f = Some FnReadParam ->
      pp_statement ppf body
      (* Skip For loop part, just emit body due to the way FnReadParam emits *)
  | For {loopvar; lower; upper; body} ->
      pp_for_loop ppf (loopvar, lower, upper, pp_statement, body)
  | Profile (name, ls) -> pp_profile ppf (pp_stmt_list, name, ls)
  | Block ls -> pp_block ppf (pp_stmt_list, ls)
  | SList ls -> pp_stmt_list ppf ls
  | Decl {decl_adtype; decl_id; decl_type} ->
      pp_possibly_sized_decl ppf (decl_id, decl_type, decl_adtype)

and pp_block_s ppf body =
  match body.pattern with
  | Block ls -> pp_block ppf (list ~sep:cut pp_statement, ls)
  | _ -> pp_block ppf (pp_statement, body)
