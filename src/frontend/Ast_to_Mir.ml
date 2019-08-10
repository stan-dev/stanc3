open Core_kernel
open Middle
open Expr_helpers
open Stmt_helpers

let loop_bottom = loop_bottom Expr.Typed.Meta.empty

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (UnsizedType.ReturnType ut) -> ut
  | x ->
      raise_s
        [%message
          "Unexpected return type " (x : UnsizedType.returntype option)]

let trans_fn_kind = function
  | Ast.StanLib -> Fun_kind.StanLib
  | UserDefined -> UserDefined

let rec op_to_funapp op args =
  let argtypes =
    List.map ~f:(fun x -> (x.Ast.emeta.Ast.ad_level, x.emeta.type_)) args
  in
  let type_ = Stan_math.op_return_type op argtypes |> unwrap_return_exn
  and loc = Ast.expr_loc_lub args
  and adlevel = Ast.expr_ad_lub args in
  let meta = Expr.Typed.Meta.create ~type_ ~loc ~adlevel () in
  stanlib_fun meta (Operator.to_string op) @@ trans_exprs args

and trans_expr {Ast.expr; Ast.emeta} =
  let type_ = emeta.Ast.type_
  and loc = emeta.loc
  and adlevel = emeta.ad_level in
  let meta = Expr.Typed.Meta.create ~type_ ~loc ~adlevel () in
  match expr with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, And, rhs) -> and_ meta (trans_expr lhs) (trans_expr rhs)
  | BinOp (lhs, Or, rhs) -> or_ meta (trans_expr lhs) (trans_expr rhs)
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs]
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e]
  | TernaryIf (cond, ifb, elseb) ->
      ternary_if meta (trans_expr cond) (trans_expr ifb) (trans_expr elseb)
  | Variable {name; _} -> var meta name
  | IntNumeral x -> lit meta Int x
  | RealNumeral x -> lit meta Real x
  | FunApp (fn_kind, {name; _}, args) | CondDistApp (fn_kind, {name; _}, args)
    ->
      fun_app meta (trans_fn_kind fn_kind) name (trans_exprs args)
  | GetLP | GetTarget -> stanlib_fun meta "target" []
  | ArrayExpr eles -> internal_fun meta FnMakeArray @@ trans_exprs eles
  | RowVectorExpr eles -> internal_fun meta FnMakeRowVec @@ trans_exprs eles
  | Indexed (lhs, indices) ->
      indexed meta (trans_expr lhs) (List.map ~f:trans_idx indices)

and trans_idx = function
  | Ast.All -> Expr.All
  | Ast.Upfrom e -> Expr.Upfrom (trans_expr e)
  | Ast.Downfrom e -> Expr.Between (loop_bottom, trans_expr e)
  | Ast.Between (lb, ub) -> Expr.Between (trans_expr lb, trans_expr ub)
  | Ast.Single e -> (
    match e.emeta.type_ with
    | UInt -> Expr.Single (trans_expr e)
    | UArray _ -> Expr.MultiIndex (trans_expr e)
    | _ ->
        raise_s
          [%message "Expecting int or array" (e.emeta.type_ : UnsizedType.t)] )

and trans_exprs = List.map ~f:trans_expr

let trans_sizedtype = SizedType.map trans_expr

let neg_inf =
  let meta =
    Expr.Typed.Meta.create ~type_:UnsizedType.UReal ~loc:Location_span.empty
      ~adlevel:UnsizedType.DataOnly ()
  in
  stanlib_fun meta (Internal_fun.to_string FnNegInf) []

let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ast_obs t =
  let trunc cond_op (x : Ast.typed_expression) y =
    let emeta = x.Ast.emeta.loc in
    if_ emeta (op_to_funapp cond_op [ast_obs; x]) (target_pe emeta neg_inf) y
  in
  match t with
  | Ast.NoTruncate -> []
  | TruncateUpFrom lb -> [trunc Less lb None]
  | TruncateDownFrom ub -> [trunc Greater ub None]
  | TruncateBetween (lb, ub) -> [trunc Less lb (Some (trunc Greater ub None))]

let unquote s =
  if s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.drop_suffix (String.drop_prefix s 1) 1
  else s

(* hack(sean): strings aren't real
   XXX add UString to MIR and maybe AST.
*)
let mkstring loc s =
  let meta =
    Expr.Typed.Meta.create ~type_:UReal ~loc ~adlevel:UnsizedType.DataOnly ()
  in
  lit_string meta s

let trans_printables mloc (ps : Ast.typed_expression Ast.printable list) =
  List.map
    ~f:(function
      | Ast.PString s -> mkstring mloc (unquote s)
      | Ast.PExpr e -> trans_expr e)
    ps

(** [add_index expression index] returns an expression that (additionally)
    indexes into the input [expression] by [index].*)
let add_int_index e i =
  let loc = Expr.Typed.loc_of e in
  let type_ =
    Semantic_check.inferred_unsizedtype_of_indexed_exn ~loc
      (Expr.Typed.type_of e) [(i, UInt)]
  and adlevel = Expr.Typed.adlevel_of e
  and mir_i = trans_idx i in
  let meta = Expr.Typed.Meta.create ~type_ ~loc ~adlevel () in
  match Expr.Fixed.pattern e with
  | Var _ -> indexed meta e [mir_i]
  | Indexed (e, indices) -> indexed meta e @@ indices @ [mir_i]
  | _ -> raise_s [%message "These should go away with Ryan's LHS"]

(** [mkfor] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
let mkfor upper bodyfn iteratee smeta =
  let idx s =
    Ast.Single
      (Ast.mk_typed_expression
         ~expr:(Ast.Variable {name= s; id_loc= smeta})
         ~loc:smeta ~type_:UInt ~ad_level:DataOnly)
  in
  let loopvar, reset = Common.Gensym.enter () in
  let lower = loop_bottom in
  let body = bodyfn (add_int_index iteratee (idx loopvar)) in
  reset () ;
  Stmt_helpers.(for_ smeta loopvar lower upper @@ block smeta [body])

(** [for_scalar unsizedtype...] generates a For statement that loops
    over the scalars in the underlying [unsizedtype].

    We can call [bodyfn] directly on scalars, make a direct For loop
    around Eigen types, or for Arrays we call mkfor but inserting a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_scalar st bodyfn var smeta =
  match st with
  | SizedType.SInt | SReal -> bodyfn var
  | SVector d | SRowVector d -> mkfor d bodyfn var smeta
  | SMatrix (d1, d2) ->
      mkfor d1 (fun e -> for_scalar (SVector d2) bodyfn e smeta) var smeta
  | SArray (t, d) -> mkfor d (fun e -> for_scalar t bodyfn e smeta) var smeta

(** [for_eigen unsizedtype...] generates a For statement that loops
    over the eigen types in the underlying [unsizedtype]; i.e. just iterating
    overarrays and running bodyfn on any eign types found within.

    We can call [bodyfn] directly on scalars and Eigen types;
    for Arrays we call mkfor but insert a
    recursive call into the [bodyfn] that will operate on the nested
    type. In this way we recursively create for loops that loop over
    the outermost layers first.
*)
let rec for_eigen st bodyfn var smeta =
  match st with
  | SizedType.SInt | SReal | SVector _ | SRowVector _ | SMatrix _ -> bodyfn var
  | SArray (t, d) -> mkfor d (fun e -> for_eigen t bodyfn e smeta) var smeta

(* These types signal the context for a declaration during statement translation.
   They are only interpreted by trans_decl.*)
type ioaction = ReadData | ReadParam [@@deriving sexp]
type constrainaction = Check | Constrain | Unconstrain [@@deriving sexp]

let constrainaction_fn = function
  | Check -> Internal_fun.FnCheck
  | Constrain -> FnConstrain
  | Unconstrain -> FnUnconstrain

type decl_context =
  { dread: ioaction option
  ; dconstrain: constrainaction option
  ; dadlevel: UnsizedType.autodifftype }

let rec unsizedtype_to_string = function
  | UnsizedType.UMatrix -> "matrix"
  | UVector -> "vector"
  | URowVector -> "row_vector"
  | UReal -> "scalar"
  | UInt -> "integer"
  | UArray t -> unsizedtype_to_string t
  | t ->
      raise_s
        [%message "Another place where it's weird to get " (t : UnsizedType.t)]

let constraint_to_string t (c : constrainaction) =
  match t with
  | Ast.Ordered -> "ordered"
  | PositiveOrdered -> "positive_ordered"
  | Simplex -> "simplex"
  | UnitVector -> "unit_vector"
  | CholeskyCorr -> "cholesky_corr"
  | CholeskyCov -> "cholesky_factor"
  | Correlation -> "corr_matrix"
  | Covariance -> "cov_matrix"
  | Lower _ -> (
    match c with
    | Check -> "greater_or_equal"
    | Constrain | Unconstrain -> "lb" )
  | Upper _ -> (
    match c with Check -> "less_or_equal" | Constrain | Unconstrain -> "ub" )
  | LowerUpper _ -> (
    match c with
    | Check ->
        raise_s
          [%message "LowerUpper is really two other checks tied together"]
    | Constrain | Unconstrain -> "lub" )
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> (
    match c with Check -> "" | Constrain | Unconstrain -> "offset_multiplier" )
  | Identity -> ""

let constraint_forl = function
  | Ast.Identity | Offset _ | Ast.Multiplier _ | Ast.OffsetMultiplier _
   |Lower _ | Upper _ | LowerUpper _ ->
      for_scalar
  | Ordered | PositiveOrdered | Simplex | UnitVector | CholeskyCorr
   |CholeskyCov | Correlation | Covariance ->
      for_eigen

let rec eigen_size (st : Expr.Typed.t SizedType.t) =
  match st with
  | SArray (t, _) -> eigen_size t
  | SMatrix (d1, d2) -> [d1; d2]
  | SRowVector dim | SVector dim -> [dim]
  | SInt | SReal -> []

let extract_transform_args = function
  | Ast.Lower a | Upper a | Offset a | Multiplier a -> [a]
  | LowerUpper (a1, a2) | OffsetMultiplier (a1, a2) -> [a1; a2]
  | Covariance | Correlation | CholeskyCov | CholeskyCorr | Ordered
   |PositiveOrdered | Simplex | UnitVector | Identity ->
      []

let extra_constraint_args st = function
  | Ast.Lower _ | Upper _ | Offset _ | Multiplier _ | LowerUpper _
   |OffsetMultiplier _ | Ordered | PositiveOrdered | Simplex | UnitVector
   |Identity ->
      []
  | Covariance | Correlation | CholeskyCorr -> [List.hd_exn (eigen_size st)]
  | CholeskyCov -> eigen_size st

let rec base_type = function
  | SizedType.SArray (t, _) -> base_type t
  | SVector _ | SRowVector _ | SMatrix _ -> UnsizedType.UReal
  | x -> SizedType.to_unsizedtype x

let internal_of_dread = function
  | ReadParam -> Internal_fun.FnReadParam
  | ReadData -> FnReadData

let assign_indexed vident meta varfn var =
  let idxs = indices_of var in
  assign meta vident ~idxs @@ varfn var

let param_size transform sizedtype =
  let rec shrink_eigen f st =
    match st with
    | SizedType.SArray (t, d) -> SizedType.SArray (shrink_eigen f t, d)
    | SVector d | SMatrix (d, _) -> SVector (f d)
    | SInt | SReal | SRowVector _ ->
        raise_s
          [%message
            "Expecting SVector or SMatrix, got " (st : Expr.Typed.t SizedType.t)]
  in
  let rec shrink_eigen_mat f st =
    match st with
    | SizedType.SArray (t, d) -> SizedType.SArray (shrink_eigen_mat f t, d)
    | SMatrix (d1, d2) -> SVector (f d1 d2)
    | SInt | SReal | SRowVector _ | SVector _ ->
        raise_s
          [%message "Expecting SMatrix, got " (st : Expr.Typed.t SizedType.t)]
  in
  let int num = Expr.(lit_int Typed.Meta.empty num) in
  let k_choose_2 k =
    Expr.(
      binop Typed.Meta.empty Divide
        (binop Typed.Meta.empty Times k
           (binop Typed.Meta.empty Minus k (int 1)))
        (int 2))
  in
  match transform with
  | Ast.Identity | Lower _ | Upper _
   |LowerUpper (_, _)
   |Offset _ | Multiplier _
   |OffsetMultiplier (_, _)
   |Ordered | PositiveOrdered | UnitVector ->
      sizedtype
  | Simplex ->
      shrink_eigen
        (fun d -> Expr.(binop Typed.Meta.empty Minus d (int 1)))
        sizedtype
  | CholeskyCorr | Correlation -> shrink_eigen k_choose_2 sizedtype
  | CholeskyCov ->
      (* (N * (N + 1)) / 2 + (M - N) * N *)
      shrink_eigen_mat
        (fun m n ->
          Expr.(
            binop Typed.Meta.empty Plus
              (binop Typed.Meta.empty Plus (k_choose_2 n) n)
              (binop Typed.Meta.empty Times
                 (binop Typed.Meta.empty Minus m n)
                 n)) )
        sizedtype
  | Covariance ->
      shrink_eigen
        (fun k -> Expr.(binop Typed.Meta.empty Plus k (k_choose_2 k)))
        sizedtype

let read_decl dread decl_id sizedtype smeta decl_var =
  let args =
    [ mkstring smeta decl_id
    ; mkstring smeta (unsizedtype_to_string (Expr.Typed.type_of decl_var)) ]
    @ eigen_size sizedtype
  in
  let readfname = internal_of_dread dread in
  let readfn var =
    let meta =
      Expr.Fixed.meta var |> Expr.Typed.Meta.with_type (base_type sizedtype)
    in
    internal_fun meta readfname args
  in
  let readvar var =
    match Expr.Fixed.pattern var with
    | Indexed (_, idxs) ->
        let meta = Expr.Fixed.meta var in
        indexed meta (readfn var) idxs
    | _ -> readfn var
  in
  let forl, st =
    match dread with
    | ReadData -> (for_scalar, sizedtype)
    | ReadParam -> (for_eigen, sizedtype)
  in
  forl st (assign_indexed decl_id smeta readvar) decl_var smeta

let constrain_decl st dconstrain t decl_id decl_var smeta =
  let mkstring = mkstring (Expr.Typed.loc_of decl_var) in
  match Option.map ~f:(constraint_to_string t) dconstrain with
  | None | Some "" -> []
  | Some constraint_str ->
      let dc = Option.value_exn dconstrain in
      let fn = constrainaction_fn dc in
      let extra_args =
        match dconstrain with
        | Some Constrain -> extra_constraint_args st t
        | _ -> []
      in
      let args var =
        (var :: mkstring constraint_str :: extract_transform_args t)
        @ extra_args
      in
      let constrainvar var =
        let meta = Expr.Fixed.meta var in
        internal_fun meta fn @@ args var
      in
      [ (constraint_forl t) st
          (assign_indexed decl_id smeta constrainvar)
          decl_var smeta ]

let rec check_decl decl_type decl_id decl_trans smeta adlevel =
  let chk fn args =
    let check_id id =
      let id_str =
        lit_string Expr.Typed.Meta.empty @@ Fmt.strf "%a" Expr.Typed.pp id
      in
      internal_nrfun smeta FnCheck @@ (fn :: id_str :: id :: args)
    in
    let type_ = SizedType.to_unsizedtype decl_type in
    for_eigen decl_type check_id
      Expr.(var (Typed.Meta.create ~type_ ~loc:smeta ~adlevel ()) decl_id)
      smeta
  in
  let args = extract_transform_args decl_trans in
  match decl_trans with
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier (_, _) -> []
  | LowerUpper (lb, ub) ->
      check_decl decl_type decl_id (Ast.Lower lb) smeta adlevel
      @ check_decl decl_type decl_id (Ast.Upper ub) smeta adlevel
  | _ -> [chk (mkstring smeta (constraint_to_string decl_trans Check)) args]

let trans_decl {dread; dconstrain; dadlevel} smeta sizedtype transform
    identifier initial_value =
  let decl_id = identifier.Ast.name in
  let rhs = Option.map ~f:trans_expr initial_value in
  let dt = trans_sizedtype sizedtype in
  let decl_adtype = dadlevel in
  let decl_var =
    let meta =
      Expr.Typed.Meta.create
        ~type_:(SizedType.to_unsizedtype sizedtype)
        ~adlevel:dadlevel ~loc:smeta ()
    in
    var meta decl_id
  in
  let decl = declare_sized smeta decl_adtype decl_id dt in
  let checks =
    match dconstrain with
    | Some Check -> check_decl dt decl_id transform smeta dadlevel
    | _ -> []
  in
  let (temp_decl_id, temp_decl_var, temp_dt), unconstrained_decl =
    let unconstrained_decl =
      match transform with
      | Ast.Identity | Ast.Lower _ | Ast.Upper _
       |Ast.LowerUpper (_, _)
       |Ast.Offset _ | Ast.Multiplier _
       |Ast.OffsetMultiplier (_, _)
       |Ast.Ordered | Ast.PositiveOrdered ->
          None
      | Ast.Simplex | Ast.UnitVector | Ast.CholeskyCorr | Ast.CholeskyCov
       |Ast.Correlation | Ast.Covariance ->
          let decl_id = decl_id ^ "_" ^ Common.Gensym.generate () in
          let st = param_size transform dt in
          let emeta =
            Expr.Fixed.meta decl_var
            |> Expr.Typed.Meta.with_type (SizedType.to_unsizedtype st)
          in
          let stmt = declare_sized smeta decl_adtype decl_id st
          and expr = var emeta decl_id in
          Some ((decl_id, expr, st), [stmt])
    in
    match (dconstrain, unconstrained_decl) with
    | Some Constrain, Some ud -> ud
    | _ -> ((decl_id, decl_var, dt), [])
  in
  let constrain_stmts =
    match dconstrain with
    | Some Constrain | Some Unconstrain ->
        constrain_decl dt dconstrain transform decl_id temp_decl_var smeta
    | _ -> []
  in
  let read_stmts =
    match (dread, rhs) with
    | Some dread, _ ->
        [read_decl dread temp_decl_id temp_dt smeta temp_decl_var]
    | None, Some e -> [assign smeta decl_id e]
    | None, None -> []
  in
  (unconstrained_decl @ (decl :: read_stmts)) @ constrain_stmts @ checks

let unwrap_block_or_skip (stmts : Stmt.Located.t list) =
  match stmts with
  | [b] -> (
    match Stmt.Fixed.pattern b with
    | Block _ | Skip -> b
    | _ ->
        raise_s
          [%message
            "Expecting a block or skip, not" (stmts : Stmt.Located.t list)] )
  | _ ->
      raise_s
        [%message
          "Expecting a block or skip, not" (stmts : Stmt.Located.t list)]

let rec trans_stmt (declc : decl_context) (ts : Ast.typed_statement) =
  let stmt_typed = ts.stmt and smeta = ts.smeta.loc in
  let trans_stmt = trans_stmt {declc with dread= None; dconstrain= None} in
  let trans_single_stmt s = trans_stmt s |> List.hd_exn in
  let mloc = smeta in
  match stmt_typed with
  | Ast.Assignment
      { assign_lhs=
          { assign_identifier
          ; assign_indices
          ; assign_meta= {id_ad_level; id_type_; lhs_ad_level; lhs_type_; loc}
          }
      ; assign_rhs
      ; assign_op } ->
      let assignee =
        { Ast.expr=
            ( match assign_indices with
            | [] -> Ast.Variable assign_identifier
            | _ ->
                Ast.Indexed
                  ( { expr= Ast.Variable assign_identifier
                    ; emeta=
                        { Ast.loc= Location_span.empty
                        ; ad_level= id_ad_level
                        ; type_= id_type_ } }
                  , assign_indices ) )
        ; emeta= {Ast.loc; ad_level= lhs_ad_level; type_= lhs_type_} }
      in
      let rhs =
        match assign_op with
        | Ast.Assign | Ast.ArrowAssign -> trans_expr assign_rhs
        | Ast.OperatorAssign op -> op_to_funapp op [assignee; assign_rhs]
      in
      [ assign smeta assign_identifier.name
          ~idxs:(List.map ~f:trans_idx assign_indices)
          rhs ]
  | Ast.NRFunApp (fn_kind, {name; _}, args) ->
      [nrfun_app smeta (trans_fn_kind fn_kind) name (trans_exprs args)]
  | Ast.IncrementLogProb e | Ast.TargetPE e -> [target_pe smeta @@ trans_expr e]
  | Ast.Tilde {arg; distribution; args; truncation} ->
      let suffix = Stan_math.distribution_name_suffix distribution.name in
      let name =
        distribution.name ^ Utils.proportional_to_distribution_infix ^ suffix
      in
      let add_dist : Stmt.Located.t =
        target_pe smeta
          Expr.(
            stanlib_fun
              (Typed.Meta.create ~loc:mloc
                 ~adlevel:(Ast.expr_ad_lub (arg :: args))
                 ~type_:UnsizedType.ureal ())
              name
            @@ trans_exprs (arg :: args))
      in
      truncate_dist arg truncation @ [add_dist]
  | Ast.Print ps -> [internal_nrfun smeta FnPrint @@ trans_printables smeta ps]
  | Ast.Reject ps ->
      [internal_nrfun smeta FnReject @@ trans_printables smeta ps]
  | Ast.IfThenElse (cond, ifb, elseb) ->
      [ if_ smeta (trans_expr cond) (trans_single_stmt ifb)
          (Option.map ~f:trans_single_stmt elseb) ]
  | Ast.While (cond, body) ->
      [while_ smeta (trans_expr cond) (trans_single_stmt body)]
  | Ast.For {loop_variable; lower_bound; upper_bound; loop_body} ->
      let body = trans_single_stmt loop_body |> lift_to_block in
      [ for_ smeta loop_variable.Ast.name (trans_expr lower_bound)
          (trans_expr upper_bound) body ]
  | Ast.ForEach (loopvar, iteratee, body) ->
      let newsym = Common.Gensym.generate () in
      let emeta =
        Expr.Typed.Meta.create ~loc:mloc ~type_:UInt ~adlevel:DataOnly ()
      in
      let iteratee = trans_expr iteratee and indexing_var = var emeta newsym in
      let indices =
        let single_one = (Ast.Single (Ast.IntNumeral "1"), UnsizedType.UInt) in
        match Expr.Typed.type_of iteratee with
        | UMatrix -> [single_one; single_one]
        | _ -> [single_one]
      in
      let decl_type =
        Semantic_check.inferred_unsizedtype_of_indexed_exn
          ~loc:(Expr.Typed.loc_of iteratee)
          (Expr.Typed.type_of iteratee)
          indices
      in
      let decl_loopvar =
        declare_unsized smeta
          (Expr.Typed.adlevel_of iteratee)
          loopvar.name decl_type
      in
      let assign_loopvar =
        assign smeta loopvar.name
        @@ index_single emeta iteratee ~idx:indexing_var
      in
      let body_stmts = trans_single_stmt body |> block_statements in
      let body = block smeta (decl_loopvar :: assign_loopvar :: body_stmts) in
      let upper = internal_fun emeta FnLength [iteratee] in
      [for_ smeta newsym loop_bottom upper body]
  | Ast.FunDef _ ->
      raise_s
        [%message
          "Found function definition statement outside of function block"]
  | Ast.VarDecl
      {sizedtype; transformation; identifier; initial_value; is_global} ->
      ignore is_global ;
      trans_decl declc smeta sizedtype
        (Ast.map_transformation trans_expr transformation)
        identifier initial_value
  | Ast.Block stmts -> [block smeta @@ List.concat_map ~f:trans_stmt stmts]
  | Ast.Return e -> [return_ smeta @@ trans_expr e]
  | Ast.ReturnVoid -> [return_void smeta]
  | Ast.Break -> [break smeta]
  | Ast.Continue -> [continue smeta]
  | Ast.Skip -> [skip smeta]

let trans_fun_def (ts : Ast.typed_statement) =
  match ts.stmt with
  | Ast.FunDef {returntype; funname; arguments; body} ->
      { Program.fdrt=
          (match returntype with Void -> None | ReturnType ut -> Some ut)
      ; fdname= funname.name
      ; fdargs= List.map ~f:trans_arg arguments
      ; fdbody=
          trans_stmt
            {dread= None; dconstrain= None; dadlevel= AutoDiffable}
            body
          |> unwrap_block_or_skip
      ; fdloc= ts.smeta.loc }
  | _ ->
      raise_s
        [%message "Found non-function definition statement in function block"]

let gen_write decl_id sizedtype =
  let bodyfn var = internal_nrfun Location_span.empty FnWriteParam [var] in
  let meta =
    Expr.Typed.Meta.(empty |> with_type (SizedType.to_unsizedtype sizedtype))
  in
  let expr = var meta decl_id in
  for_scalar sizedtype bodyfn expr Location_span.empty

let gen_writes block_filter vars =
  List.filter_map
    ~f:(function
      | decl_id, {Program.out_block; out_constrained_st; _}
        when out_block = block_filter ->
          Some (gen_write decl_id out_constrained_st)
      | _ -> None)
    vars

let compiler_if compiler_internal_var stmts =
  let body =
    match stmts with
    | [x] when is_block x -> x
    | _ -> block Location_span.empty stmts
  in
  let cond = var Expr.Typed.Meta.empty compiler_internal_var in
  match stmts with [] -> [] | _ -> [if_ Location_span.empty cond body None]

let get_block block prog =
  match block with
  | Program.Parameters -> prog.Ast.parametersblock
  | TransformedParameters -> prog.transformedparametersblock
  | GeneratedQuantities -> prog.generatedquantitiesblock

let migrate_checks_to_end_of_block stmts =
  let checks, not_checks =
    List.partition_tf stmts ~f:(contains_internal_fun ~fn:FnCheck)
  in
  not_checks @ checks

let trans_prog filename p : Program.Typed.t =
  (*
     1. prepare_params: add read_param calls (same call should constrain?)
          maybe read(constrained()), constrain(read()), or read("constraint", ...)
     1. prepare_params: add tparams 's; add checks
     2. transform_inits: add read_param calls (same call should unconstrain?)
     3. prepare_data: add read_data calls and checks
     4. prepare_data: add tdata 's and checks
     5. add write() calls to generate_quantities for params, tparams...
             shit these are conditional depending on the flag.
             add the flag to the call to write?
           apparently tdata aren't written anywhere

     during code gen:
     get_param_names: scan prepare_params for Decl at top level
     constrained_param_names: needs to tell between tparams and gqs and not
     unconstrained param names: same, but also some funky
        adjustments for unconstrained space: ???
*)
  let { Ast.functionblock
      ; datablock
      ; transformeddatablock
      ; parametersblock
      ; transformedparametersblock
      ; modelblock; _ } =
    p
  in
  let map f list_op = Option.value ~default:[] list_op |> List.concat_map ~f in
  let get_name_size s =
    match s.Ast.stmt with
    | Ast.VarDecl {sizedtype; identifier; transformation; _} ->
        [(identifier.name, trans_sizedtype sizedtype, transformation)]
    | _ -> []
  in
  let grab_names_sizes block =
    List.map ~f:get_name_size (Option.value ~default:[] (get_block block p))
    |> List.concat_map
         ~f:
           (List.map ~f:(fun (n, s, t) ->
                ( n
                , { Program.out_constrained_st= s
                  ; out_unconstrained_st= param_size t s
                  ; out_block= block } ) ))
  in
  let output_vars =
    [ grab_names_sizes Parameters
    ; grab_names_sizes TransformedParameters
    ; grab_names_sizes GeneratedQuantities ]
    |> List.concat
  and input_vars =
    map get_name_size datablock |> List.map ~f:(fun (n, st, _) -> (n, st))
  in
  let datab =
    map
      (trans_stmt
         {dread= Some ReadData; dconstrain= Some Check; dadlevel= DataOnly})
      datablock
    |> migrate_checks_to_end_of_block
  in
  let prepare_data =
    datab
    @ map
        (trans_stmt {dread= None; dconstrain= Some Check; dadlevel= DataOnly})
        transformeddatablock
    |> migrate_checks_to_end_of_block
  in
  let modelb =
    map
      (trans_stmt {dread= None; dconstrain= None; dadlevel= AutoDiffable})
      modelblock
  in
  let log_prob =
    map
      (trans_stmt
         { dread= Some ReadParam
         ; dconstrain= Some Constrain
         ; dadlevel= AutoDiffable })
      parametersblock
    @ ( map
          (trans_stmt
             {dread= None; dconstrain= Some Check; dadlevel= AutoDiffable})
          transformedparametersblock
      |> migrate_checks_to_end_of_block )
    @
    match modelb with
    | [] -> []
    | hd :: _ ->
        let meta = Stmt.Fixed.meta hd in
        [block meta modelb]
  in
  let gen_from_block declc block =
    map (trans_stmt declc) (get_block block p) @ gen_writes block output_vars
  in
  let txparam_decls, txparam_stmts =
    gen_from_block
      {dread= None; dconstrain= None; dadlevel= DataOnly}
      TransformedParameters
    |> List.partition_tf ~f:is_decl
  in
  let generate_quantities =
    gen_from_block
      {dread= Some ReadParam; dconstrain= Some Constrain; dadlevel= DataOnly}
      Parameters
    @ txparam_decls
    @ compiler_if
        "emit_transformed_parameters__ || emit_generated_quantities__"
        txparam_stmts
    @ compiler_if "emit_generated_quantities__"
        (migrate_checks_to_end_of_block
           (gen_from_block
              {dread= None; dconstrain= Some Check; dadlevel= DataOnly}
              GeneratedQuantities))
  in
  let transform_inits =
    gen_from_block
      {dread= Some ReadData; dconstrain= Some Unconstrain; dadlevel= DataOnly}
      Parameters
  in
  { functions_block=
      Option.value_map functionblock ~default:[] ~f:(List.map ~f:trans_fun_def)
  ; input_vars
  ; prepare_data
  ; log_prob
  ; generate_quantities
  ; transform_inits
  ; output_vars
  ; prog_name= !Semantic_check.model_name
  ; prog_path= filename }
