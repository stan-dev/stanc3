(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel
open Mir

let rec subst (m : (string, expr) Map.Poly.t) (e : expr) =
  match e with
  | Var s -> ( match Map.find m s with Some e' -> e' | None -> e )
  | Lit (_, _) -> e
  | FunApp (f, l) -> FunApp (f, List.map ~f:(subst m) l)
  | BinOp (e1, op, e2) -> BinOp (subst m e1, op, subst m e2)
  | TernaryIf (e1, e2, e3) -> TernaryIf (subst m e1, subst m e2, subst m e3)
  | Indexed (e, l) -> Indexed (subst m e, List.map ~f:(subst_idx m) l)

and subst_idx m i =
  match i with
  | All -> All
  | Single e -> Single (subst m e)
  | Upfrom e -> Upfrom (subst m e)
  | Downfrom e -> Downfrom (subst m e)
  | Between (e1, e2) -> Between (subst m e1, subst m e2)
  | MultiIndex e -> MultiIndex (subst m e)

(* TODO *)
let rec eval (e : expr) =
  match e with
  | Var _ | Lit (_, _) -> e
  | FunApp (f, l) -> FunApp (f, List.map ~f:eval l)
  | BinOp (e1, op, e2) -> (
    match (eval e1, op, eval e2) with
    | Lit (Int, i1), Plus, Lit (Int, i2) ->
        Lit (Int, Int.to_string (Int.of_string i1 + Int.of_string i2))
        (* TODO: etc *)
    | (e1', _, e2') -> BinOp (e1', op, e2') )
  | TernaryIf (e1, e2, e3) -> TernaryIf (eval e1, eval e2, eval e3)
  | Indexed (e, l) -> Indexed (eval e, List.map ~f:eval_idx l)

and eval_idx i =
  match i with
  | All -> All
  | Single e -> Single (eval e)
  | Upfrom e -> Upfrom (eval e)
  | Downfrom e -> Downfrom (eval e)
  | Between (e1, e2) -> Between (eval e1, eval e2)
  | MultiIndex e -> MultiIndex (eval e)

let eval_subst (m : (string, expr) Map.Poly.t) (e : expr) = eval (subst m e)
