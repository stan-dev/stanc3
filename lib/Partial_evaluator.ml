(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel

(* TODO *)
let rec subst (m : (string, Mir.expr) Map.Poly.t) (e : Mir.expr) =
  match e with
  | Mir.Var s -> ( match Map.find m s with Some e' -> e' | None -> e )
  | Mir.Lit (_, _) -> e
  | Mir.FunApp (f, l) -> Mir.FunApp (f, List.map ~f:(subst m) l)
  | Mir.BinOp (e1, op, e2) -> Mir.BinOp (subst m e1, op, subst m e2)
  | Mir.TernaryIf (e1, e2, e3) ->
      Mir.TernaryIf (subst m e1, subst m e2, subst m e3)
  | Mir.Indexed (e, l) -> Mir.Indexed (subst m e, List.map ~f:(subst_idx m) l)

and subst_idx m i =
  match i with
  | Mir.All -> Mir.All
  | Mir.Single e -> Mir.Single (subst m e)
  | Mir.Upfrom e -> Mir.Upfrom (subst m e)
  | Mir.Downfrom e -> Mir.Downfrom (subst m e)
  | Mir.Between (e1, e2) -> Mir.Between (subst m e1, subst m e2)
  | Mir.MultiIndex e -> Mir.MultiIndex (subst m e)

(* TODO *)
let eval (e : Mir.expr) = e

let eval_subst (m : (string, Mir.expr) Map.Poly.t) (e : Mir.expr) =
  eval (subst m e)
