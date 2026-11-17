open Std
open Middle
open Mir_utils

type step = Subscript of Expr.Typed.t Index.t | Field of int
type path = step list
type location = {variable: string; path: path}
type accesses = {reads: location list; writes: location list}

let empty = {reads= []; writes= []}
let add_read location effects = {effects with reads= location :: effects.reads}

let add_write location effects =
  {effects with writes= location :: effects.writes}

let is_single_element = function
  | Subscript (Index.Single _) | Field _ -> true
  | Subscript _ -> false

let rec expression_path (expr : Expr.Typed.t) suffix =
  match expr.pattern with
  | Var variable -> Some {variable; path= suffix}
  | Indexed (base, indices) ->
      let steps = List.map indices ~f:(fun index -> Subscript index) in
      if
        (not (List.is_empty suffix))
        && not (List.for_all steps ~f:is_single_element)
      then None (* Indexing a slice cannot be flattened into a base path. *)
      else expression_path base (steps @ suffix)
  | TupleProjection (base, field) -> expression_path base (Field field :: suffix)
  | FunApp _ | TernaryIf _ | Lit _ | EAnd _ | EOr _ | Promotion _ -> None

let rec lvalue_path ((base, indices) : Expr.Typed.t Stmt.Pattern.lvalue) suffix
    =
  let indexed_suffix =
    List.map indices ~f:(fun index -> Subscript index) @ suffix in
  match base with
  | LVariable _ -> indexed_suffix
  | LTupleProjection (inner, field) ->
      lvalue_path inner (Field field :: indexed_suffix)

let index_expressions path =
  List.concat_map path ~f:(function
    | Subscript index -> Index.bounds index
    | Field _ -> [])

let is_target_increment = function
  | Fun_kind.UserDefined (_, (FnTarget | FnJacobian)) | StanLib (_, FnJacobian)
    ->
      true
  | _ -> false

let target = {variable= "target"; path= []}
let add_target_read_write effects = add_write target (add_read target effects)

let rec collect_expr effects (expr : Expr.Typed.t) =
  match expr.pattern with
  | Var variable -> add_read {variable; path= []} effects
  | Indexed _ | TupleProjection _ -> (
      match expression_path expr [] with
      | Some location ->
          List.fold_left
            (index_expressions location.path)
            ~init:(add_read location effects)
            ~f:collect_expr
      | None -> collect_subexprs effects expr)
  | FunApp (StanLib (_, FnTarget), []) -> add_read target effects
  | FunApp (kind, _) when is_target_increment kind ->
      add_target_read_write (collect_subexprs effects expr)
  | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
      collect_subexprs effects expr

and collect_subexprs effects (expr : Expr.Typed.t) =
  Expr.Pattern.fold collect_expr effects expr.pattern

let collect_accesses (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) =
  match stmt with
  | Assignment (lhs, _, rhs) ->
      let path = lvalue_path lhs [] in
      let effects =
        List.fold_left (index_expressions path) ~init:empty ~f:collect_expr
      in
      add_write
        {variable= Stmt.Helpers.lhs_variable lhs; path}
        (collect_expr effects rhs)
  | _ -> (
      let effects = Stmt.Pattern.fold collect_expr Fun.const empty stmt in
      match stmt with
      | Decl {decl_id; _} -> add_write {variable= decl_id; path= []} effects
      | For {loopvar; _} -> add_write {variable= loopvar; path= []} effects
      | TargetPE _ | JacobianPE _ -> add_target_read_write effects
      | NRFunApp (kind, _) when is_target_increment kind ->
          add_target_read_write effects
      | _ -> effects)

let literal_value (expr : Expr.Typed.t) =
  match expr.pattern with
  | Lit (Int, digits) -> Int.of_string_opt digits
  | _ -> None

let rec paths_may_overlap first second =
  match (first, second) with
  | Field a :: first_tail, Field b :: second_tail ->
      a = b && paths_may_overlap first_tail second_tail
  | Subscript (Single a) :: first_tail, Subscript (Single b) :: second_tail ->
      let indices_may_equal =
        match (literal_value a, literal_value b) with
        | Some left, Some right -> left = right
        | _ -> true in
      indices_may_equal && paths_may_overlap first_tail second_tail
  | _ -> true

let may_overlap (first : path) (second : path) =
  List.compare_lengths first second <> 0 || paths_may_overlap first second

let is_stable defined_vars expr =
  (not (cannot_duplicate_expr expr))
  && Set.Poly.disjoint defined_vars (expr_var_names_set expr)

let rec definitely_overwrites defined_vars (killer : path) (killed : path) =
  match (killer, killed) with
  | [], _ -> true
  | Field a :: killer_tail, Field b :: killed_tail ->
      a = b && definitely_overwrites defined_vars killer_tail killed_tail
  | Subscript (Single a) :: killer_tail, Subscript (Single b) :: killed_tail ->
      is_stable defined_vars a
      && Expr.Helpers.compare_ignore_meta a b = 0
      && definitely_overwrites defined_vars killer_tail killed_tail
  | _ -> false
