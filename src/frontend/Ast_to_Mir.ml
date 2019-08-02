open Core_kernel
open Middle

(* XXX fix exn *)
let unwrap_return_exn = function
  | Some (ReturnType ut) -> ut
  | x -> raise_s [%message "Unexpected return type " (x : returntype option)]

let trans_fn_kind = function
  | Ast.StanLib -> StanLib
  | UserDefined -> UserDefined

let rec op_to_funapp op args =
  let argtypes =
    List.map ~f:(fun x -> (x.Ast.emeta.Ast.ad_level, x.emeta.type_)) args
  in
  { expr= FunApp (StanLib, string_of_operator op, trans_exprs args)
  ; emeta=
      { mtype= operator_return_type op argtypes |> unwrap_return_exn
      ; mloc= Ast.expr_loc_lub args
      ; madlevel= Ast.expr_ad_lub args } }

and trans_expr {Ast.expr; Ast.emeta} =
  let mtype = emeta.Ast.type_
  and mloc = emeta.loc
  and madlevel = emeta.ad_level in
  match expr with
  | Ast.Paren x -> trans_expr x
  | BinOp (lhs, And, rhs) ->
      { expr= EAnd (trans_expr lhs, trans_expr rhs)
      ; emeta= {madlevel; mloc; mtype} }
  | BinOp (lhs, Or, rhs) ->
      { expr= EOr (trans_expr lhs, trans_expr rhs)
      ; emeta= {madlevel; mloc; mtype} }
  | BinOp (lhs, op, rhs) -> op_to_funapp op [lhs; rhs]
  | PrefixOp (op, e) | Ast.PostfixOp (e, op) -> op_to_funapp op [e]
  | _ ->
      let expr =
        match expr with
        | Ast.TernaryIf (cond, ifb, elseb) ->
            TernaryIf (trans_expr cond, trans_expr ifb, trans_expr elseb)
        | Variable {name; _} -> Var name
        | IntNumeral x -> Lit (Int, x)
        | RealNumeral x -> Lit (Real, x)
        | FunApp (fn_kind, {name; _}, args)
         |CondDistApp (fn_kind, {name; _}, args) ->
            FunApp (trans_fn_kind fn_kind, name, trans_exprs args)
        | GetLP | GetTarget -> FunApp (StanLib, "target", [])
        | ArrayExpr eles ->
            FunApp
              ( CompilerInternal
              , string_of_internal_fn FnMakeArray
              , trans_exprs eles )
        | RowVectorExpr eles ->
            FunApp
              ( CompilerInternal
              , string_of_internal_fn FnMakeRowVec
              , trans_exprs eles )
        | Indexed (lhs, indices) ->
            Indexed (trans_expr lhs, List.map ~f:trans_idx indices)
        | Paren _ | BinOp _ | PrefixOp _ | PostfixOp _ ->
            raise_s [%message "Impossible!"]
      in
      {expr; emeta= {mtype; mloc; madlevel}}

and trans_idx = function
  | Ast.All -> All
  | Ast.Upfrom e -> Upfrom (trans_expr e)
  | Ast.Downfrom e -> Between (loop_bottom, trans_expr e)
  | Ast.Between (lb, ub) -> Between (trans_expr lb, trans_expr ub)
  | Ast.Single e -> (
    match e.emeta.type_ with
    | UInt -> Single (trans_expr e)
    | UArray _ -> MultiIndex (trans_expr e)
    | _ ->
        raise_s
          [%message "Expecting int or array" (e.emeta.type_ : unsizedtype)] )

and trans_exprs = List.map ~f:trans_expr

let trans_sizedtype = map_sizedtype trans_expr
let trans_possiblysizedtype pst = map_possiblysizedtype trans_expr pst

let neg_inf =
  { expr= FunApp (StanLib, string_of_internal_fn FnNegInf, [])
  ; emeta= {mtype= UReal; mloc= no_span; madlevel= DataOnly} }

let trans_arg (adtype, ut, ident) = (adtype, ident.Ast.name, ut)

let truncate_dist ast_obs t =
  let trunc cond_op (x : Ast.typed_expression) y =
    let smeta = x.Ast.emeta.loc in
    { smeta
    ; stmt=
        IfElse
          ( op_to_funapp cond_op [ast_obs; x]
          , {smeta; stmt= TargetPE neg_inf}
          , y ) }
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
let mkstring mloc s =
  {expr= Lit (Str, s); emeta= {mtype= UReal; mloc; madlevel= DataOnly}}

let trans_printables mloc (ps : Ast.typed_expression Ast.printable list) =
  List.map
    ~f:(function
      | Ast.PString s -> mkstring mloc (unquote s)
      | Ast.PExpr e -> trans_expr e)
    ps

(** [add_index expression index] returns an expression that (additionally)
    indexes into the input [expression] by [index].*)
let add_int_index e i =
  let mtype =
    Semantic_check.inferred_unsizedtype_of_indexed_exn ~loc:e.emeta.mloc
      e.emeta.mtype [i]
  and mir_i = trans_idx i in
  let expr =
    match e.expr with
    | Var _ -> Indexed (e, [mir_i])
    | Indexed (e, indices) -> Indexed (e, indices @ [mir_i])
    | _ -> raise_s [%message "These should go away with Ryan's LHS"]
  in
  {expr; emeta= {e.emeta with mtype}}

(** [mkfor] returns a MIR For statement that iterates over the given expression
    [iteratee]. *)
let mkfor upper bodyfn iteratee smeta =
  let idx s =
    Ast.Single
      (Ast.mk_typed_expression
         ~expr:(Ast.Variable {name= s; id_loc= smeta})
         ~loc:smeta ~type_:UInt ~ad_level:DataOnly)
  in
  let loopvar, reset = gensym_enter () in
  let lower = loop_bottom in
  let stmt = Block [bodyfn (add_int_index iteratee (idx loopvar))] in
  reset () ;
  {stmt= For {loopvar; lower; upper; body= {stmt; smeta}}; smeta}

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
  | SInt | SReal -> bodyfn var
  | SVector d | SRowVector d -> mkfor d bodyfn var smeta
  | SMatrix (d1, d2) ->
      mkfor d1 (fun e -> for_scalar (SRowVector d2) bodyfn e smeta) var smeta
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
  | SInt | SReal | SVector _ | SRowVector _ | SMatrix _ -> bodyfn var
  | SArray (t, d) -> mkfor d (fun e -> for_eigen t bodyfn e smeta) var smeta

(* These types signal the context for a declaration during statement translation.
   They are only interpreted by trans_decl.*)
type ioaction = ReadData | ReadParam [@@deriving sexp]
type constrainaction = Check | Constrain | Unconstrain [@@deriving sexp]

let constrainaction_fname c =
  string_of_internal_fn
    ( match c with
    | Check -> FnCheck
    | Constrain -> FnConstrain
    | Unconstrain -> FnUnconstrain )

type decl_context =
  { dread: ioaction option
  ; dconstrain: constrainaction option
  ; dadlevel: autodifftype }

let rec unsizedtype_to_string = function
  | UMatrix -> "matrix"
  | UVector -> "vector"
  | URowVector -> "row_vector"
  | UReal -> "scalar"
  | UInt -> "integer"
  | UArray t -> unsizedtype_to_string t
  | t ->
      raise_s
        [%message "Another place where it's weird to get " (t : unsizedtype)]

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

let rec eigen_size (st : mtype_loc_ad with_expr sizedtype) =
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
  | SArray (t, _) -> base_type t
  | SVector _ | SRowVector _ | SMatrix _ -> UReal
  | x -> remove_size x

let internal_of_dread = function
  | ReadParam -> FnReadParam
  | ReadData -> FnReadData

let rec pull_indices {expr; _} =
  match expr with
  | Indexed (obj, indices) -> pull_indices obj @ indices
  | _ -> []

let assign_indexed decl_type vident smeta varfn var =
  let indices = pull_indices var in
  {stmt= Assignment ((vident, decl_type, indices), varfn var); smeta}

let param_size transform sizedtype =
  let rec shrink_eigen f st =
    match st with
    | SArray (t, d) -> SArray (shrink_eigen f t, d)
    | SVector d | SMatrix (d, _) -> SVector (f d)
    | SInt | SReal | SRowVector _ ->
        raise_s
          [%message
            "Expecting SVector or SMatrix, got "
              (st : mtype_loc_ad with_expr sizedtype)]
  in
  let rec shrink_eigen_mat f st =
    match st with
    | SArray (t, d) -> SArray (shrink_eigen_mat f t, d)
    | SMatrix (d1, d2) -> SVector (f d1 d2)
    | SInt | SReal | SRowVector _ | SVector _ ->
        raise_s
          [%message
            "Expecting SMatrix, got " (st : mtype_loc_ad with_expr sizedtype)]
  in
  let int num = {expr= Lit (Int, string_of_int num); emeta= internal_meta} in
  let k_choose_2 k =
    binop (binop k Times (binop k Minus (int 1))) Divide (int 2)
  in
  match transform with
  | Ast.Identity | Lower _ | Upper _
   |LowerUpper (_, _)
   |Offset _ | Multiplier _
   |OffsetMultiplier (_, _)
   |Ordered | PositiveOrdered | UnitVector ->
      sizedtype
  | Simplex -> shrink_eigen (fun d -> binop d Minus (int 1)) sizedtype
  | CholeskyCorr | Correlation -> shrink_eigen k_choose_2 sizedtype
  | CholeskyCov ->
      (* (N * (N + 1)) / 2 + (M - N) * N *)
      shrink_eigen_mat
        (fun m n ->
          binop
            (binop (k_choose_2 n) Plus n)
            Plus
            (binop (binop m Minus n) Times n) )
        sizedtype
  | Covariance -> shrink_eigen (fun k -> binop k Plus (k_choose_2 k)) sizedtype

let remove_possibly_exn pst action loc =
  match pst with
  | Sized st -> st
  | Unsized _ ->
      raise_s
        [%message
          "Error extracting sizedtype" ~action ~loc:(loc : location_span)]

(* mat_to_arr is used whenever we have a Stan matrix but we're dealing with
   an underlying array. This is a code smell - not sure how to fix it short of
   complete refactoring of data input.*)
let rec mat_to_arr = function
  | SMatrix (d1, d2) -> SArray (SRowVector d2, d1)
  | SArray (t, d) -> SArray (mat_to_arr t, d)
  | st -> st

let read_decl dread decl_id decl_type smeta decl_var =
  let sizedtype = remove_possibly_exn decl_type "read" smeta in
  let args =
    [ mkstring smeta decl_id
    ; mkstring smeta (unsizedtype_to_string decl_var.emeta.mtype) ]
    @ eigen_size sizedtype
  in
  let readfname = internal_of_dread dread in
  let readfn var =
    internal_funapp readfname args {var.emeta with mtype= base_type sizedtype}
  in
  let rec readvar var =
    match var.expr with
    | Var id when id = decl_id -> readfn var
    | e -> {var with expr= map_expr readvar e}
  in
  let forl =
    match dread with
    | ReadData -> for_scalar (mat_to_arr sizedtype)
    | ReadParam -> for_eigen sizedtype
  in
  forl
    (assign_indexed (remove_size sizedtype) decl_id smeta readvar)
    decl_var smeta

let constrain_decl decl_type dconstrain t decl_id decl_var smeta =
  let st = remove_possibly_exn decl_type "constrain" smeta in
  let mkstring = mkstring decl_var.emeta.mloc in
  match Option.map ~f:(constraint_to_string t) dconstrain with
  | None | Some "" -> []
  | Some constraint_str ->
      let dc = Option.value_exn dconstrain in
      let fname = constrainaction_fname dc in
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
        {expr= FunApp (CompilerInternal, fname, args var); emeta= var.emeta}
      in
      [ (constraint_forl t) st
          (assign_indexed (remove_size st) decl_id smeta constrainvar)
          decl_var smeta ]

let rec check_decl decl_type' decl_id decl_trans smeta adlevel =
  let decl_type = remove_possibly_exn decl_type' "check" smeta in
  let chk fn args =
    let check_id id =
      let id_str =
        { expr= Lit (Str, Fmt.strf "%a" Pretty.pp_expr_typed_located id)
        ; emeta= internal_meta }
      in
      let fname = string_of_internal_fn FnCheck in
      let stmt =
        NRFunApp (CompilerInternal, fname, fn :: id_str :: id :: args)
      in
      {stmt; smeta}
    in
    let mtype = remove_size decl_type in
    for_eigen decl_type check_id
      {expr= Var decl_id; emeta= {mtype; mloc= smeta; madlevel= adlevel}}
      smeta
  in
  let args = extract_transform_args decl_trans in
  match decl_trans with
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier (_, _) -> []
  | LowerUpper (lb, ub) ->
      check_decl decl_type' decl_id (Ast.Lower lb) smeta adlevel
      @ check_decl decl_type' decl_id (Ast.Upper ub) smeta adlevel
  | _ -> [chk (mkstring smeta (constraint_to_string decl_trans Check)) args]

let trans_decl {dread; dconstrain; dadlevel} smeta decl_type transform
    identifier initial_value =
  let decl_id = identifier.Ast.name in
  let rhs = Option.map ~f:trans_expr initial_value in
  let dt = trans_possiblysizedtype decl_type in
  let decl_adtype = dadlevel in
  let decl_var =
    { expr= Var decl_id
    ; emeta=
        {mtype= remove_possible_size decl_type; madlevel= dadlevel; mloc= smeta}
    }
  in
  let decl = {stmt= Decl {decl_adtype; decl_id; decl_type= dt}; smeta} in
  let rhs_assignment =
    Option.map
      ~f:(fun e -> {stmt= Assignment ((decl_id, e.emeta.mtype, []), e); smeta})
      rhs
    |> Option.to_list
  in
  if String.is_suffix ~suffix:"__" decl_id then decl :: rhs_assignment
  else
    let checks =
      match dconstrain with
      | Some Check -> check_decl dt decl_id transform smeta dadlevel
      | _ -> []
    in
    let (temp_decl_id, temp_decl_var, temp_dt), unconstrained_decl =
      let default = ((decl_id, decl_var, dt), []) in
      match dconstrain with
      | Some Constrain -> (
        match transform with
        | Ast.Identity | Ast.Lower _ | Ast.Upper _
         |Ast.LowerUpper (_, _)
         |Ast.Offset _ | Ast.Multiplier _
         |Ast.OffsetMultiplier (_, _)
         |Ast.Ordered | Ast.PositiveOrdered ->
            default
        | Ast.Simplex | Ast.UnitVector | Ast.CholeskyCorr | Ast.CholeskyCov
         |Ast.Correlation | Ast.Covariance ->
            let dt =
              remove_possibly_exn dt "constrain" smeta |> param_size transform
            in
            let decl_id = decl_id ^ "_" ^ gensym () in
            let emeta = {decl_var.emeta with mtype= remove_size dt} in
            let stmt = Decl {decl_adtype; decl_id; decl_type= Sized dt} in
            ((decl_id, {expr= Var decl_id; emeta}, Sized dt), [{stmt; smeta}])
        )
      | _ -> default
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
      | None, Some _ -> rhs_assignment
      | None, None -> []
    in
    (unconstrained_decl @ (decl :: read_stmts)) @ constrain_stmts @ checks

let unwrap_block_or_skip = function
  | [({stmt= Block _; _} as b)] | [({stmt= Skip; _} as b)] -> b
  | x ->
      raise_s [%message "Expecting a block or skip, not" (x : stmt_loc list)]

let rec trans_stmt (declc : decl_context) (ts : Ast.typed_statement) =
  let stmt_typed = ts.stmt and smeta = ts.smeta.loc in
  let trans_stmt = trans_stmt {declc with dread= None; dconstrain= None} in
  let trans_single_stmt s = trans_stmt s |> List.hd_exn in
  let swrap stmt = [{stmt; smeta}] in
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
                        { Ast.loc= no_span
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
      Assignment
        ( ( assign_identifier.name
          , id_type_
          , List.map ~f:trans_idx assign_indices )
        , rhs )
      |> swrap
  | Ast.NRFunApp (fn_kind, {name; _}, args) ->
      NRFunApp (trans_fn_kind fn_kind, name, trans_exprs args) |> swrap
  | Ast.IncrementLogProb e | Ast.TargetPE e -> TargetPE (trans_expr e) |> swrap
  | Ast.Tilde {arg; distribution; args; truncation} ->
      let suffix = stan_distribution_name_suffix distribution.name in
      let name =
        distribution.name ^ Utils.proportional_to_distribution_infix ^ suffix
      in
      let add_dist =
        TargetPE
          { expr= FunApp (StanLib, name, trans_exprs (arg :: args))
          ; emeta= {mloc; madlevel= Ast.expr_ad_lub (arg :: args); mtype= UReal}
          }
      in
      truncate_dist arg truncation @ [{smeta; stmt= add_dist}]
  | Ast.Print ps ->
      NRFunApp
        ( CompilerInternal
        , string_of_internal_fn FnPrint
        , trans_printables smeta ps )
      |> swrap
  | Ast.Reject ps ->
      NRFunApp
        ( CompilerInternal
        , string_of_internal_fn FnReject
        , trans_printables smeta ps )
      |> swrap
  | Ast.IfThenElse (cond, ifb, elseb) ->
      IfElse
        ( trans_expr cond
        , trans_single_stmt ifb
        , Option.map ~f:trans_single_stmt elseb )
      |> swrap
  | Ast.While (cond, body) ->
      While (trans_expr cond, trans_single_stmt body) |> swrap
  | Ast.For {loop_variable; lower_bound; upper_bound; loop_body} ->
      let body =
        match trans_single_stmt loop_body with
        | {stmt= Block _; _} as b -> b
        | x -> {x with stmt= Block [x]}
      in
      For
        { loopvar= loop_variable.Ast.name
        ; lower= trans_expr lower_bound
        ; upper= trans_expr upper_bound
        ; body }
      |> swrap
  | Ast.ForEach (loopvar, iteratee, body) ->
      let newsym = gensym () in
      let wrap expr = {expr; emeta= {mloc; mtype= UInt; madlevel= DataOnly}} in
      let iteratee' = trans_expr iteratee
      and indexing_var = wrap (Var newsym) in
      let indices =
        let single_one =
          Ast.Single
            { Ast.expr= Ast.IntNumeral "1"
            ; emeta= {iteratee.emeta with type_= UInt} }
        in
        match iteratee'.emeta.mtype with
        | UMatrix -> [single_one; single_one]
        | _ -> [single_one]
      in
      let decl_type =
        Semantic_check.inferred_unsizedtype_of_indexed_exn
          ~loc:iteratee'.emeta.mloc iteratee'.emeta.mtype indices
      in
      let decl_loopvar =
        Decl
          { decl_adtype= iteratee'.emeta.madlevel
          ; decl_id= loopvar.name
          ; decl_type= Unsized decl_type }
      in
      let decl_loopvar = {stmt= decl_loopvar; smeta} in
      let assign_loopvar =
        Assignment
          ( (loopvar.name, UInt, [])
          , Indexed (iteratee', [Single indexing_var]) |> wrap )
      in
      let assign_loopvar = {stmt= assign_loopvar; smeta} in
      let body_stmts =
        match trans_single_stmt body with
        | {stmt= Block body_stmts; _} -> body_stmts
        | b -> [b]
      in
      let body =
        {stmt= Block (decl_loopvar :: assign_loopvar :: body_stmts); smeta}
      in
      For
        { loopvar= newsym
        ; lower= loop_bottom
        ; upper=
            wrap
            @@ FunApp (StanLib, string_of_internal_fn FnLength, [iteratee'])
        ; body }
      |> swrap
  | Ast.FunDef _ ->
      raise_s
        [%message
          "Found function definition statement outside of function block"]
  | Ast.VarDecl
      {decl_type; transformation; identifier; initial_value; is_global} ->
      ignore is_global ;
      trans_decl declc smeta decl_type
        (Ast.map_transformation trans_expr transformation)
        identifier initial_value
  | Ast.Block stmts -> Block (List.concat_map ~f:trans_stmt stmts) |> swrap
  | Ast.Return e -> Return (Some (trans_expr e)) |> swrap
  | Ast.ReturnVoid -> Return None |> swrap
  | Ast.Break -> Break |> swrap
  | Ast.Continue -> Continue |> swrap
  | Ast.Skip -> Skip |> swrap

let trans_fun_def (ts : Ast.typed_statement) =
  match ts.stmt with
  | Ast.FunDef {returntype; funname; arguments; body} ->
      { fdrt= (match returntype with Void -> None | ReturnType ut -> Some ut)
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
  let bodyfn var =
    { stmt=
        NRFunApp (CompilerInternal, string_of_internal_fn FnWriteParam, [var])
    ; smeta= no_span }
  in
  for_scalar sizedtype bodyfn
    { expr= Var decl_id
    ; emeta= {internal_meta with mtype= remove_size sizedtype} }
    no_span

let gen_writes block_filter vars =
  List.filter_map
    ~f:(function
      | decl_id, {out_block; out_constrained_st; _}
        when out_block = block_filter ->
          Some (gen_write decl_id out_constrained_st)
      | _ -> None)
    vars

let compiler_if compiler_internal_var stmts =
  let body =
    match stmts with
    | [({stmt= Block _; _} as s)] -> s
    | ls -> {stmt= Block ls; smeta= no_span}
  in
  let cond = {expr= Var compiler_internal_var; emeta= internal_meta} in
  match stmts with
  | [] -> []
  | _ -> [{stmt= IfElse (cond, body, None); smeta= no_span}]

let get_block block prog =
  match block with
  | Parameters -> prog.Ast.parametersblock
  | TransformedParameters -> prog.transformedparametersblock
  | GeneratedQuantities -> prog.generatedquantitiesblock

let migrate_checks_to_end_of_block stmts =
  let is_check = contains_fn (string_of_internal_fn FnCheck) in
  let checks, not_checks = List.partition_tf ~f:is_check stmts in
  not_checks @ checks

let trans_prog filename (p : Ast.typed_program) : typed_prog =
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
    | Ast.VarDecl {decl_type= Sized st; identifier; transformation; _} ->
        [(identifier.name, trans_sizedtype st, transformation)]
    | _ -> []
  in
  let grab_names_sizes block =
    List.map ~f:get_name_size (Option.value ~default:[] (get_block block p))
    |> List.concat_map
         ~f:
           (List.map ~f:(fun (n, s, t) ->
                ( n
                , { out_constrained_st= s
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
  let declc = {dread= None; dconstrain= None; dadlevel= DataOnly} in
  let datab =
    map
      (trans_stmt {declc with dread= Some ReadData; dconstrain= Some Check})
      datablock
    |> migrate_checks_to_end_of_block
  in
  let prepare_data =
    datab
    @ map (trans_stmt {declc with dconstrain= Some Check}) transformeddatablock
    |> migrate_checks_to_end_of_block
  in
  let modelb =
    map (trans_stmt {declc with dadlevel= AutoDiffable}) modelblock
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
             {declc with dconstrain= Some Check; dadlevel= AutoDiffable})
          transformedparametersblock
      |> migrate_checks_to_end_of_block )
    @
    match modelb with
    | [] -> []
    | hd :: _ -> [{stmt= Block modelb; smeta= hd.smeta}]
  in
  let gen_from_block declc block =
    map (trans_stmt declc) (get_block block p) @ gen_writes block output_vars
  in
  let txparam_decls, txparam_stmts =
    gen_from_block declc TransformedParameters
    |> List.partition_tf ~f:(function {stmt= Decl _; _} -> true | _ -> false)
  in
  let generate_quantities =
    gen_from_block
      {declc with dread= Some ReadParam; dconstrain= Some Constrain}
      Parameters
    @ txparam_decls
    @ compiler_if
        "emit_transformed_parameters__ || emit_generated_quantities__"
        txparam_stmts
    @ compiler_if "emit_generated_quantities__"
        (migrate_checks_to_end_of_block
           (gen_from_block
              {declc with dconstrain= Some Check}
              GeneratedQuantities))
  in
  let transform_inits =
    gen_from_block
      {declc with dread= Some ReadData; dconstrain= Some Unconstrain}
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
