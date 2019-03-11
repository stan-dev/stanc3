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

let rec eval (e : expr) =
  match e with
  | Var _ | Lit (_, _) -> e
  | FunApp (f, l) -> (
    match (f, List.map ~f:eval l) with
    (* TODO: deal with tilde statements and unnormalized distributions properly here *)
    (* TODO: deal with GLM functions here *)
    (* TODO: be careful here with operators which get translated to function calls *)
    | "bernoulli_lpmf", [y; FunApp ("inv_logit", [alpha])] ->
        FunApp ("bernoulli_logit_lpmf", [y; alpha])
    | "bernoulli_rng", [FunApp ("inv_logit", [alpha])] ->
        FunApp ("bernoulli_logit_rng", [alpha])
    | "binomial_lpmf", [y; FunApp ("inv_logit", [n; alpha])] ->
        FunApp ("binomial_logit_lpmf", [y; n; alpha])
    | "categorical_lpmf", [y; FunApp ("inv_logit", [alpha])] ->
        FunApp ("categorical_logit_lpmf", [y; alpha])
    | "categorical_rng", [FunApp ("inv_logit", [alpha])] ->
        FunApp ("categorical_logit_rng", [alpha])
    | "dot_product", [x; y] when x = y -> FunApp ("dot_self", [x])
    | "inv", [FunApp ("sqrt", l)] -> FunApp ("inv_sqrt", l)
    | "inv", [FunApp ("square", [x])] -> FunApp ("inv_square", [x])
    | "log", [BinOp (Lit (Int, "1"), Minus, FunApp ("exp", [x]))] ->
        FunApp ("log1m_exp", [x])
    | "log", [BinOp (Lit (Int, "1"), Minus, FunApp ("inv_logit", [x]))] ->
        FunApp ("log1m_inv_logit", [x])
    | "log", [BinOp (Lit (Int, "1"), Minus, x)] -> FunApp ("log1m", [x])
    | "log", [BinOp (Lit (Int, "1"), Plus, FunApp ("exp", [x]))] ->
        FunApp ("log1p_exp", [x])
    | "log", [BinOp (Lit (Int, "1"), Plus, x)] -> FunApp ("log1p", [x])
    | "log", [FunApp ("fabs", [FunApp ("determinant", [x])])] ->
        FunApp ("log_determinant", [x])
    (* TODO: can only do below for reals:
    | "log", [BinOp (FunApp ("exp", [x]),Minus,FunApp ("exp", [y]))] ->
    FunApp ("log_diff_exp", [x;y]) *)
    | "log", [FunApp ("falling_factorial", l)] ->  FunApp("log_falling_factorial", l)
    | "log", [FunApp ("inv_logit", l)] -> FunApp ("log_inv_logit", l)
    (* TODO: can only do below for reals:
    | "log", [BinOp (FunApp ("exp", [x]),Plus ,FunApp ("exp", [y]))] ->
    FunApp ("log_sum_exp", [x;y]) *)
    | "pow", [Lit (Int, "2"); x] -> FunApp ("exp2", [x])
    | "pow", [x; Lit (Int, "2")] ->
        FunApp ("square", [x]) (* TODO: insert all composite functions here *)
    | _, l' -> FunApp (f, l') )
  | BinOp (e1, op, e2) -> (
    match (eval e1, op, eval e2) with
    | e1', Times, FunApp ("diag_matrix", [v]) ->
        FunApp ("diag_post_multiply", [e1'; v])
    | FunApp ("diag_matrix", [v]), Times, e2' ->
        FunApp ("diag_pre_multiply", [v; e2'])
    | Lit (Int, "1"), Minus, FunApp ("erf", l) -> FunApp ("erfc", l)
    | Lit (Int, "1"), Minus, FunApp ("erfc", l) -> FunApp ("erf", l)
    | FunApp ("exp", l'), Minus, Lit (Int, "1") -> FunApp ("expm1", l')
    (* TODO: can only do below for reals:
    | BinOp (x, Times, y), Plus, z
    | z, Plus, BinOp (x, Times,y)-> FunApp ("fma", [x;y;z]) *)
    | Lit (Int, "1"), Minus, FunApp ("gamma_p", l) ->
        FunApp ("gamma_q", l)
        (* TODO: insert all composite functions here *)
        
        (* TODO: deal properly with different orders for operators, real vs int *)
    | Lit (Int, i1), Plus, Lit (Int, i2) ->
        Lit (Int, Int.to_string (Int.of_string i1 + Int.of_string i2))
        (* TODO: constant folding for arithmetic expressions *)
    | e1', _, e2' -> BinOp (e1', op, e2') )
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
