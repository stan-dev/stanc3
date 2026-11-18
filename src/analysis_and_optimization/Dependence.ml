open Std
open Middle

type access_path = Expr.Typed.t Index.t list
type location = {variable: string; path: access_path}
type accesses = {reads: location list; writes: location list}

module Location = struct
  let same_var (first : location) (second : location) =
    String.equal first.variable second.variable
end

let empty = {reads= []; writes= []}

let add_read (read_location : location) (collected_accesses : accesses) =
  {collected_accesses with reads= read_location :: collected_accesses.reads}

let add_write (write_location : location) (collected_accesses : accesses) =
  {collected_accesses with writes= write_location :: collected_accesses.writes}

let add_read_write (updated_location : location) (collected_accesses : accesses)
    =
  collected_accesses |> add_read updated_location |> add_write updated_location

let rec location_of_expr_opt (expr : Expr.Typed.t)
    (trailing_indices : access_path) =
  match expr.pattern with
  | Var variable -> Some {variable; path= trailing_indices}
  | Indexed (base, indices) ->
      if
        (not (List.is_empty trailing_indices))
        && not (List.for_all indices ~f:Index.is_single)
      then None (* Indexing a slice cannot be flattened into a base path. *)
      else location_of_expr_opt base (indices @ trailing_indices)
  | TupleProjection (base, field) ->
      location_of_expr_opt base
        (Index.Single (Expr.Helpers.int field) :: trailing_indices)
  | FunApp _ | TernaryIf _ | Lit _ | EAnd _ | EOr _ | Promotion _ -> None

let rec location_of_lvalue ((base, indices) : Expr.Typed.t Stmt.Pattern.lvalue)
    (trailing_indices : access_path) =
  let combined_indices = indices @ trailing_indices in
  match base with
  | LVariable variable -> {variable; path= combined_indices}
  | LTupleProjection (inner, field) ->
      location_of_lvalue inner
        (Index.Single (Expr.Helpers.int field) :: combined_indices)

let target = {variable= "target"; path= []}

let rec collect_expr_accesses (collected_accesses : accesses)
    (expr : Expr.Typed.t) =
  match expr.pattern with
  | Var variable -> add_read {variable; path= []} collected_accesses
  | Indexed _ | TupleProjection _ -> (
      match location_of_expr_opt expr [] with
      | Some read_location ->
          List.fold_left read_location.path
            ~init:(add_read read_location collected_accesses)
            ~f:(Index.fold collect_expr_accesses)
      | None -> collect_subexpr_accesses collected_accesses expr)
  | FunApp (StanLib (_, FnTarget), []) -> add_read target collected_accesses
  | FunApp (kind, _) when Fun_kind.is_target_increment kind ->
      collect_subexpr_accesses collected_accesses expr |> add_read_write target
  | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
      collect_subexpr_accesses collected_accesses expr

and collect_subexpr_accesses collected_accesses (expr : Expr.Typed.t) =
  Expr.Pattern.fold collect_expr_accesses collected_accesses expr.pattern

let collect_accesses (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) =
  match stmt with
  | Assignment (lhs, _, rhs) ->
      let write_location = location_of_lvalue lhs [] in
      let index_accesses =
        List.fold_left write_location.path ~init:empty
          ~f:(Index.fold collect_expr_accesses) in
      collect_expr_accesses index_accesses rhs |> add_write write_location
  | _ -> (
      let expression_accesses =
        Stmt.Pattern.fold collect_expr_accesses Fun.const empty stmt in
      match stmt with
      | Decl {decl_id; _} ->
          add_write {variable= decl_id; path= []} expression_accesses
      | For {loopvar; _} ->
          add_write {variable= loopvar; path= []} expression_accesses
      | TargetPE _ | JacobianPE _ -> add_read_write target expression_accesses
      | NRFunApp (kind, _) when Fun_kind.is_target_increment kind ->
          add_read_write target expression_accesses
      | _ -> expression_accesses)

(* Some 0 means always even; Some 1 means always odd. None permits both. Keeping
   only parity avoids arithmetic overflow in the analysis. *)
let rec integer_parity (expr : Expr.Typed.t) =
  match expr.pattern with
  | Lit (Int, _) ->
      Option.map (Expr.Helpers.int_literal_value expr) ~f:(fun value ->
          abs (value mod 2))
  | FunApp (Operator (PPlus | PMinus), [operand]) -> integer_parity operand
  | FunApp (Operator (Plus | Minus), [left; right]) -> (
      match (integer_parity left, integer_parity right) with
      | Some left_parity, Some right_parity ->
          Some ((left_parity + right_parity) mod 2)
      | None, _ | _, None -> None)
  | FunApp (Operator Times, [left; right]) -> (
      match (integer_parity left, integer_parity right) with
      | Some 0, _ | _, Some 0 -> Some 0
      | Some left_parity, Some right_parity -> Some (left_parity * right_parity)
      | None, _ | _, None -> None)
  | Var _ | Indexed _ | TupleProjection _ | FunApp _ | TernaryIf _ | Lit _
   |EAnd _ | EOr _ | Promotion _ ->
      None

let may_equal_int first_expr second_expr =
  if Expr.Helpers.compare_ignore_meta first_expr second_expr = 0 then true
  else
    match
      ( Expr.Helpers.int_literal_value first_expr
      , Expr.Helpers.int_literal_value second_expr )
    with
    | Some first_value, Some second_value -> first_value = second_value
    | Some _, None | None, Some _ | None, None -> (
        match (integer_parity first_expr, integer_parity second_expr) with
        | Some first_parity, Some second_parity -> first_parity = second_parity
        (* An unknown parity permits either even or odd values. *)
        | Some _, None | None, Some _ | None, None -> true)

let may_be_less_or_equal first_expr second_expr =
  match
    ( Expr.Helpers.int_literal_value first_expr
    , Expr.Helpers.int_literal_value second_expr )
  with
  | Some first_value, Some second_value -> first_value <= second_value
  (* Without a value for either bound, their order is not established. *)
  | Some _, None | None, Some _ | None, None -> true

let is_empty_index = function
  | Index.Between (lower, upper) -> (
      match
        ( Expr.Helpers.int_literal_value lower
        , Expr.Helpers.int_literal_value upper )
      with
      | Some lower_value, Some upper_value -> lower_value > upper_value
      | _ -> false)
  | MultiIndex array_expr -> (
      match Expr.Helpers.array_literal_elements array_expr with
      | Some [] -> true
      | _ -> false)
  | All | Single _ | Upfrom _ -> false

let rec may_indices_overlap (first_index : Expr.Typed.t Index.t)
    (second_index : Expr.Typed.t Index.t) : bool =
  if is_empty_index first_index || is_empty_index second_index then false
  else
    match first_index with
    | All -> true
    | Single first_value -> (
        match second_index with
        | All -> true
        | Single second_value -> may_equal_int first_value second_value
        | Upfrom lower -> may_be_less_or_equal lower first_value
        | Between (lower, upper) ->
            may_be_less_or_equal lower first_value
            && may_be_less_or_equal first_value upper
        | MultiIndex second_array ->
            array_may_overlap_index second_array first_index)
    | Upfrom first_lower -> (
        match second_index with
        | All -> true
        | Single second_value -> may_be_less_or_equal first_lower second_value
        | Upfrom _ ->
            (* Both nonempty suffixes contain the dimension's final element. *)
            true
        | Between (_, second_upper) ->
            may_be_less_or_equal first_lower second_upper
        | MultiIndex second_array ->
            array_may_overlap_index second_array first_index)
    | Between (first_lower, first_upper) -> (
        match second_index with
        | All -> true
        | Single second_value ->
            may_be_less_or_equal first_lower second_value
            && may_be_less_or_equal second_value first_upper
        | Upfrom second_lower -> may_be_less_or_equal second_lower first_upper
        | Between (second_lower, second_upper) ->
            may_be_less_or_equal second_lower first_upper
            && may_be_less_or_equal first_lower second_upper
        | MultiIndex second_array ->
            array_may_overlap_index second_array first_index)
    | MultiIndex first_array -> (
        match second_index with
        | All -> true
        | Single _ -> array_may_overlap_index first_array second_index
        | Upfrom _ -> array_may_overlap_index first_array second_index
        | Between (_, _) -> array_may_overlap_index first_array second_index
        | MultiIndex _ -> array_may_overlap_index first_array second_index)

and array_may_overlap_index array_expr other_index =
  match Expr.Helpers.array_literal_elements array_expr with
  | None ->
      (* A runtime index array can contain an element selected by
         other_index. *)
      true
  | Some elements ->
      List.exists elements ~f:(fun element ->
          may_indices_overlap (Index.Single element) other_index)

let may_overlap (first_path : access_path) (second_path : access_path) : bool =
  let rec paths_overlap remaining_first remaining_second =
    match (remaining_first, remaining_second) with
    | first_index :: first_tail, second_index :: second_tail ->
        may_indices_overlap first_index second_index
        && paths_overlap first_tail second_tail
    | _ -> true in
  List.compare_lengths first_path second_path <> 0
  || paths_overlap first_path second_path

let rec is_path_overwritten (written_vars : string Set.Poly.t)
    (previous_path : access_path) (write_path : access_path) : bool =
  match (previous_path, write_path) with
  | _, [] -> true
  | ( Index.Single previous_index :: previous_tail
    , Index.Single write_index :: write_tail ) ->
      Mir_utils.is_stable_expr written_vars write_index
      && Expr.Helpers.compare_ignore_meta write_index previous_index = 0
      && is_path_overwritten written_vars previous_tail write_tail
  | _ -> false

let is_overwritten (written_vars : string Set.Poly.t) (previous : location)
    (write : location) : bool =
  Location.same_var previous write
  && is_path_overwritten written_vars previous.path write.path
