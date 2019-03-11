(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel

(* TODO *)
let subst (_ : (string, Mir.expr) Map.Poly.t) (e : Mir.expr) = e

(* TODO *)
let eval (e : Mir.expr) = e

let eval_subst (m : (string, Mir.expr) Map.Poly.t) (e : Mir.expr) =
  eval (subst m e)
