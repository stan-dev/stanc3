(** Types of indexing operations *)

open Core_kernel

type 'a t =
  | All
  | Single of 'a
  | Upfrom of 'a
  | Between of 'a * 'a
  | MultiIndex of 'a
[@@deriving sexp, hash, map, compare, fold]

let pp pp_e ppf = function
  | All -> Fmt.char ppf ':'
  | Single index -> pp_e ppf index
  | Upfrom index -> Fmt.pf ppf {|%a:|} pp_e index
  | Between (lower, upper) -> Fmt.pf ppf {|%a:%a|} pp_e lower pp_e upper
  | MultiIndex index -> Fmt.pf ppf {|%a|} pp_e index

let pp_indexed pp_e ppf (ident, indices) =
  Fmt.pf ppf {|@[%s%a@]|} ident
    ( if List.is_empty indices then fun _ _ -> ()
    else Fmt.(list (pp pp_e) ~sep:comma |> brackets) )
    indices

let bounds = function
  | All -> []
  | Single e | Upfrom e | MultiIndex e -> [e]
  | Between (e1, e2) -> [e1; e2]

(**
 Apply an op over the [Index] types inner expressions.
 @param default Value to return for [All]
 @param merge Function taking in lhs and rhs of [Between] and
 merging their result.
 @param op a functor to run with inputs of inner exprs
 @param ind the Index.t to
 *)
let apply ~default ~merge op (ind : 'a t) =
  match ind with
  | All -> default
  | Single ind_expr -> op ind_expr
  | Upfrom ind_expr -> op ind_expr
  | Between (expr_top, expr_bottom) -> merge (op expr_top) (op expr_bottom)
  | MultiIndex exprs -> op exprs

let folder (acc : string Set.Poly.t) op (ind : 'a t) : string Set.Poly.t =
  match ind with
  | All -> acc
  | Single ind_expr | Upfrom ind_expr | MultiIndex ind_expr -> op acc ind_expr
  | Between (expr_top, expr_bottom) ->
      let top_fold = op acc expr_top in
      Set.Poly.union top_fold (op top_fold expr_bottom)
