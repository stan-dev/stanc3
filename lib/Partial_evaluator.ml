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

let apply_operator_int (op : operator) i1 i2 =
  Lit
    ( Int
    , Int.to_string
        ( match op with
        | Plus -> i1 + i2
        | Minus -> i1 - i2
        | Times -> i1 * i2
        | Divide -> i1 / i2
        | Modulo -> i1 % i2
        | Or -> Bool.to_int (i1 <> 0 || i2 <> 0)
        | And -> Bool.to_int (i1 <> 0 && i2 <> 0)
        | Equals -> Bool.to_int (i1 = i2)
        | NEquals -> Bool.to_int (i1 <> i2)
        | Less -> Bool.to_int (i1 < i2)
        | Leq -> Bool.to_int (i1 <= i2)
        | Greater -> Bool.to_int (i1 > i2)
        | Geq -> Bool.to_int (i1 >= i2) ) )

let apply_arithmetic_operator_real (op : operator) r1 r2 =
  Lit
    ( Real
    , Float.to_string
        ( match op with
        | Plus -> r1 +. r2
        | Minus -> r1 -. r2
        | Times -> r1 *. r2
        | Divide -> r1 /. r2
        | _ -> Errors.fatal_error () ) )

let apply_logical_operator_real (op : operator) r1 r2 =
  Lit
    ( Int
    , Int.to_string
        ( match op with
        | Or -> Bool.to_int (r1 <> 0. || r2 <> 0.)
        | And -> Bool.to_int (r1 <> 0. && r2 <> 0.)
        | Equals -> Bool.to_int (r1 = r2)
        | NEquals -> Bool.to_int (r1 <> r2)
        | Less -> Bool.to_int (r1 < r2)
        | Leq -> Bool.to_int (r1 <= r2)
        | Greater -> Bool.to_int (r1 > r2)
        | Geq -> Bool.to_int (r1 >= r2)
        | _ -> Errors.fatal_error () ) )

let rec eval (e : expr) =
  match e with
  | Var _ | Lit (_, _) -> e
  | FunApp (f, l) -> (
    match (f, List.map ~f:eval l) with
    (* TODO: deal with tilde statements and unnormalized distributions properly here *)
    
    (* TODO: be careful here with operators which get translated to function calls *)
    (* TODO: deal with GLM functions here. Need type information though.
    | "bernoulli_logit_lpmf", [y; BinOp (alpha, Plus, BinOp (beta, Times, x))]
     |"bernoulli_logit_lpmf", [y; BinOp (BinOp (beta, Times, x), Plus, alpha)]
     |"bernoulli_logit_lpmf", [y; BinOp (alpha, Plus, BinOp (x, Times, beta))]
     |"bernoulli_logit_lpmf", [y; BinOp (BinOp (x, Times, beta), Plus, alpha)]
      ->
        FunApp ("bernoulli_logit_glm_lpmf", [y; x; alpha; beta]) *)
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
    | "columns_dot_product", [x; y] when x = y ->
        FunApp ("columns_dot_self", [x])
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
    (* TODO: log_mix?*)
    | "log", [FunApp ("falling_factorial", l)] ->
        FunApp ("log_falling_factorial", l)
    | "log", [FunApp ("rising_factorial", l)] ->
        FunApp ("log_rising_factorial", l)
    | "log", [FunApp ("inv_logit", l)] -> FunApp ("log_inv_logit", l)
    | "log", [FunApp ("softmax", l)] -> FunApp ("log_softmax", l)
    | "log", [FunApp ("sum", [FunApp ("exp", l)])] -> FunApp ("log_sum_exp", l)
    (* TODO: can only do below for reals:
    | "log", [BinOp (FunApp ("exp", [x]),Plus ,FunApp ("exp", [y]))] ->
    FunApp ("log_sum_exp", [x;y]) *)
    | "multi_normal_lpdf", [y; mu; FunApp ("inverse", [tau])] ->
        FunApp ("multi_normal_prec_lpdf", [y; mu; tau])
    | "neg_binomial_2_lpmf", [y; FunApp ("log", [eta]); phi] ->
        FunApp ("neg_binomial_2_log_lpmf", [y; eta; phi])
    | "neg_binomial_2_rng", [FunApp ("log", [eta]); phi] ->
        FunApp ("neg_binomial_2_log_rng", [eta; phi])
    | "poisson_lpmf", [y; FunApp ("log", [eta])] ->
        FunApp ("poisson_log_lpmf", [y; eta])
    | "poisson_rng", [FunApp ("log", [eta])] ->
        FunApp ("poisson_log_rng", [eta])
    | "pow", [Lit (Int, "2"); x] -> FunApp ("exp2", [x])
    | "rows_dot_product", [x; y] when x = y -> FunApp ("rows_dot_self", [x])
    | "pow", [x; Lit (Int, "2")] -> FunApp ("square", [x])
    | "pow", [x; Lit (Real, "0.5")]
     |"pow", [x; BinOp (Lit (Int, "1"), Divide, Lit (Int, "2"))] ->
        FunApp ("sqrt", [x])
    (* TODO: insert all composite functions here *)
    | "square", [FunApp ("sd", [x])] -> FunApp ("variance", [x])
    | "sqrt", [Lit (Int, "2")] -> FunApp ("sqrt2", [])
    | "sum", [FunApp ("square", [BinOp (x, Minus, y)])] ->
        FunApp ("squared_distance", [x; y])
    | "sum", [FunApp ("diagonal", l)] -> FunApp ("trace", l)
    | ( "trace"
      , [ BinOp
            ( BinOp (BinOp (d, Times, FunApp ("transpose", [b])), Times, a)
            , Times
            , c ) ] )
      when b = c ->
        FunApp ("trace_gen_quad_form", [d; a; b])
    | "trace", [FunApp ("quad_form", [a; b])] ->
        FunApp ("trace_quad_form", [a; b])
    | _, l' -> FunApp (f, l') )
  | BinOp (e1, op, e2) -> (
    match (eval e1, op, eval e2) with
    | Lit (Int, "1"), Minus, FunApp ("erf", l) -> FunApp ("erfc", l)
    | Lit (Int, "1"), Minus, FunApp ("erfc", l) -> FunApp ("erf", l)
    | FunApp ("exp", l'), Minus, Lit (Int, "1") -> FunApp ("expm1", l')
    (* TODO: can only do below for reals:
    | BinOp (x, Times, y), Plus, z
    | z, Plus, BinOp (x, Times,y)-> FunApp ("fma", [x;y;z]) *)
    | Lit (Int, "1"), Minus, FunApp ("gamma_p", l) -> FunApp ("gamma_q", l)
    | Lit (Int, "1"), Minus, FunApp ("gamma_q", l) -> FunApp ("gamma_p", l)
    (* TODO: can only do below for t reals:       
| FunApp("matrix_exp", [BinOp(t,Times ,a)]), Times, b
| FunApp("matrix_exp", [BinOp(a,Times ,t)]), Times, b-> FunApp("scale_matrix_exp_multiply", [t;a;b]) *)
    | FunApp ("matrix_exp", [a]), Times, b ->
        FunApp ("matrix_exp_multiply", [a; b])
    (* TODO: can only do below for reals:  
| x, Times, FunApp("log", [y]) -> FunApp("multiply_log", [x;y]) *)
    | ( FunApp ("transpose", [FunApp ("diag_matrix", [v])])
      , Times
      , BinOp (a, Times, FunApp ("diag_matrix", [w])) )
      when v = w ->
        FunApp ("quad_form_diag", [a; v])
    | ( BinOp (FunApp ("transpose", [FunApp ("diag_matrix", [v])]), Times, a)
      , Times
      , FunApp ("diag_matrix", [w]) )
      when v = w ->
        FunApp ("quad_form_diag", [a; v])
    | FunApp ("transpose", [b]), Times, BinOp (a, Times, c) when b = c ->
        FunApp ("quad_form", [a; b])
    | BinOp (FunApp ("transpose", [b]), Times, a), Times, c when b = c ->
        FunApp ("quad_form", [a; b])
    | e1', Times, FunApp ("diag_matrix", [v]) ->
        FunApp ("diag_post_multiply", [e1'; v])
    | FunApp ("diag_matrix", [v]), Times, e2' ->
        FunApp ("diag_pre_multiply", [v; e2'])
        (* Constant folding for operators *)
    | Lit (Int, i1), _, Lit (Int, i2) ->
        apply_operator_int op (Int.of_string i1) (Int.of_string i2)
    | Lit (Real, i1), _, Lit (Real, i2)
     |Lit (Int, i1), _, Lit (Real, i2)
     |Lit (Real, i1), _, Lit (Int, i2) -> (
      match op with
      | Plus | Minus | Times | Divide ->
          apply_arithmetic_operator_real op (Float.of_string i1)
            (Float.of_string i2)
      | _ ->
          apply_logical_operator_real op (Float.of_string i1)
            (Float.of_string i2) )
    | e1', _, e2' -> BinOp (e1', op, e2') )
  | TernaryIf (e1, e2, e3) -> (
    match (eval e1, eval e2, eval e3) with
    | Lit (Int, "0"), _, e3' -> e3'
    | Lit (Int, _), e2', _ -> e2'
    | e1', e2', e3' -> TernaryIf (e1', e2', e3') )
  | Indexed (e, l) ->
      (* TODO: do something clever with array and matrix expressions here?
  Note  that we could also constant fold array sizes if we keep those around on declarations. *)
      Indexed (eval e, List.map ~f:eval_idx l)

and eval_idx i =
  match i with
  | All -> All
  | Single e -> Single (eval e)
  | Upfrom e -> Upfrom (eval e)
  | Downfrom e -> Downfrom (eval e)
  | Between (e1, e2) -> Between (eval e1, eval e2)
  | MultiIndex e -> MultiIndex (eval e)

let eval_subst (m : (string, expr) Map.Poly.t) (e : expr) = eval (subst m e)
