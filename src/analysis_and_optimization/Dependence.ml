open Std
open Middle

type path = Expr.Typed.t Index.t list
type location = {variable: string; path: path}
type accesses = {reads: location list; writes: location list}

let empty = {reads= []; writes= []}

let add_read (location : location) (effects : accesses) =
  {effects with reads= location :: effects.reads}

let add_write (location : location) (effects : accesses) =
  {effects with writes= location :: effects.writes}

let add_read_write (location : location) (effects : accesses) =
  effects |> add_read location |> add_write location

let rec expression_path (expr : Expr.Typed.t) suffix =
  match expr.pattern with
  | Var variable -> Some {variable; path= suffix}
  | Indexed (base, indices) ->
      if
        (not (List.is_empty suffix))
        && not (List.for_all indices ~f:Index.is_single)
      then None (* Indexing a slice cannot be flattened into a base path. *)
      else expression_path base (indices @ suffix)
  | TupleProjection (base, field) ->
      expression_path base (Index.Single (Expr.Helpers.int field) :: suffix)
  | FunApp _ | TernaryIf _ | Lit _ | EAnd _ | EOr _ | Promotion _ -> None

let rec lvalue_location ((base, indices) : Expr.Typed.t Stmt.Pattern.lvalue)
    suffix =
  let indexed_suffix = indices @ suffix in
  match base with
  | LVariable variable -> {variable; path= indexed_suffix}
  | LTupleProjection (inner, field) ->
      lvalue_location inner
        (Index.Single (Expr.Helpers.int field) :: indexed_suffix)

let target = {variable= "target"; path= []}

let rec collect_expr (effects : accesses) (expr : Expr.Typed.t) =
  match expr.pattern with
  | Var variable -> add_read {variable; path= []} effects
  | Indexed _ | TupleProjection _ -> (
      match expression_path expr [] with
      | Some location ->
          List.fold_left location.path
            ~init:(add_read location effects)
            ~f:(Index.fold collect_expr)
      | None -> collect_subexprs effects expr)
  | FunApp (StanLib (_, FnTarget), []) -> add_read target effects
  | FunApp (kind, _) when Fun_kind.is_target_increment kind ->
      collect_subexprs effects expr |> add_read_write target
  | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
      collect_subexprs effects expr

and collect_subexprs effects (expr : Expr.Typed.t) =
  Expr.Pattern.fold collect_expr effects expr.pattern

let collect_accesses (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) =
  match stmt with
  | Assignment (lhs, _, rhs) ->
      let location = lvalue_location lhs [] in
      let effects =
        List.fold_left location.path ~init:empty ~f:(Index.fold collect_expr)
      in
      collect_expr effects rhs |> add_write location
  | _ -> (
      let effects = Stmt.Pattern.fold collect_expr Fun.const empty stmt in
      match stmt with
      | Decl {decl_id; _} -> add_write {variable= decl_id; path= []} effects
      | For {loopvar; _} -> add_write {variable= loopvar; path= []} effects
      | TargetPE _ | JacobianPE _ -> add_read_write target effects
      | NRFunApp (kind, _) when Fun_kind.is_target_increment kind ->
          add_read_write target effects
      | _ -> effects)

let may_overlap (first : path) (second : path) : bool =
  let rec paths_overlap remaining_first remaining_second =
    match (remaining_first, remaining_second) with
    | Index.Single a :: first_tail, Index.Single b :: second_tail ->
        let indices_may_equal =
          match
            (Expr.Helpers.int_literal_value a, Expr.Helpers.int_literal_value b)
          with
          | Some left, Some right -> left = right
          | _ -> true in
        indices_may_equal && paths_overlap first_tail second_tail
    | _ -> true in
  List.compare_lengths first second <> 0 || paths_overlap first second

let rec is_path_overwritten (defined_vars : string Set.Poly.t) (previous : path)
    (write : path) : bool =
  match (previous, write) with
  | _, [] -> true
  | ( Index.Single previous_index :: previous_tail
    , Index.Single write_index :: write_tail ) ->
      Mir_utils.is_stable_expr defined_vars write_index
      && Expr.Helpers.compare_ignore_meta write_index previous_index = 0
      && is_path_overwritten defined_vars previous_tail write_tail
  | _ -> false

let is_overwritten (defined_vars : string Set.Poly.t) (previous : location)
    (write : location) : bool =
  String.equal previous.variable write.variable
  && is_path_overwritten defined_vars previous.path write.path
