(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel
open Mir

let apply_prefix_operator_int (op : string) i =
  Lit
    ( Int
    , Int.to_string
        ( match op with
        | "PPlus__" -> i
        | "PMinus__" -> -i
        | "PNot__" -> if i = 0 then 1 else 0
        | _ -> Errors.fatal_error () ) )

let apply_prefix_operator_real (op : string) i =
  Lit
    ( Real
    , Float.to_string
        ( match op with
        | "PPlus__" -> i
        | "PMinus__" -> -.i
        | _ -> Errors.fatal_error () ) )

let apply_operator_int (op : string) i1 i2 =
  Lit
    ( Int
    , Int.to_string
        ( match op with
        | "Plus__" -> i1 + i2
        | "Minus__" -> i1 - i2
        | "Times__" -> i1 * i2
        | "Divide__" -> i1 / i2
        | "Modulo__" -> i1 % i2
        | "Or__" -> Bool.to_int (i1 <> 0 || i2 <> 0)
        | "And__" -> Bool.to_int (i1 <> 0 && i2 <> 0)
        | "Equals__" -> Bool.to_int (i1 = i2)
        | "NEquals__" -> Bool.to_int (i1 <> i2)
        | "Less__" -> Bool.to_int (i1 < i2)
        | "Leq__" -> Bool.to_int (i1 <= i2)
        | "Greater__" -> Bool.to_int (i1 > i2)
        | "Geq__" -> Bool.to_int (i1 >= i2)
        | _ -> Errors.fatal_error () ) )

let apply_arithmetic_operator_real (op : string) r1 r2 =
  Lit
    ( Real
    , Float.to_string
        ( match op with
        | "Plus__" -> r1 +. r2
        | "Minus__" -> r1 -. r2
        | "Times__" -> r1 *. r2
        | "Divide__" -> r1 /. r2
        | _ -> Errors.fatal_error () ) )

let apply_logical_operator_real (op : string) r1 r2 =
  Lit
    ( Int
    , Int.to_string
        ( match op with
        | "Or__" -> Bool.to_int (r1 <> 0. || r2 <> 0.)
        | "And__" -> Bool.to_int (r1 <> 0. && r2 <> 0.)
        | "Equals__" -> Bool.to_int (r1 = r2)
        | "NEquals__" -> Bool.to_int (r1 <> r2)
        | "Less__" -> Bool.to_int (r1 < r2)
        | "Leq__" -> Bool.to_int (r1 <= r2)
        | "Greater__" -> Bool.to_int (r1 > r2)
        | "Geq__" -> Bool.to_int (r1 >= r2)
        | _ -> Errors.fatal_error () ) )

let rec eval_expr (e : Mir.expr_typed_located) =
  { e with
    texpr=
      ( match e.texpr with
      | Var _ | Lit (_, _) -> e.texpr
      | FunApp (f, l) -> (
        match (f, List.map ~f:eval_expr l) with
        (* TODO: deal with tilde statements and unnormalized distributions properly here *)
        
        (* TODO: be careful here with operators which get translated to function calls *)
        (* TODO: deal with GLM functions here. Need type information though.
    | "bernoulli_logit_lpmf", [y; FunApp (alpha, "Plus__", FunApp (beta, "Times__", x))]
     |"bernoulli_logit_lpmf", [y; FunApp (FunApp (beta, "Times__", x), "Plus__", alpha)]
     |"bernoulli_logit_lpmf", [y; FunApp (alpha, "Plus__", FunApp (x, "Times__", beta))]
     |"bernoulli_logit_lpmf", [y; FunApp (FunApp (x, "Times__", beta), "Plus__", alpha)]
      ->
        FunApp ("bernoulli_logit_glm_lpmf", [y; x; alpha; beta]) *)
        | "bernoulli_lpmf", [y; {texpr= FunApp ("inv_logit", [alpha]); _}] ->
            FunApp ("bernoulli_logit_lpmf", [y; alpha])
        | "bernoulli_rng", [{texpr= FunApp ("inv_logit", [alpha]); _}] ->
            FunApp ("bernoulli_logit_rng", [alpha])
        | "binomial_lpmf", [y; {texpr= FunApp ("inv_logit", [n; alpha]); _}] ->
            FunApp ("binomial_logit_lpmf", [y; n; alpha])
        | "categorical_lpmf", [y; {texpr= FunApp ("inv_logit", [alpha]); _}] ->
            FunApp ("categorical_logit_lpmf", [y; alpha])
        | "categorical_rng", [{texpr= FunApp ("inv_logit", [alpha]); _}] ->
            FunApp ("categorical_logit_rng", [alpha])
            (* TODO: use compare rather than structural equality? *)
        | "columns_dot_product", [x; y] when x = y ->
            FunApp ("columns_dot_self", [x])
        | "dot_product", [x; y] when x = y -> FunApp ("dot_self", [x])
        | "inv", [{texpr= FunApp ("sqrt", l); _}] -> FunApp ("inv_sqrt", l)
        | "inv", [{texpr= FunApp ("square", [x]); _}] ->
            FunApp ("inv_square", [x])
        | ( "log"
          , [ { texpr=
                  FunApp
                    ( "Minus__"
                    , [ {texpr= Lit (Int, "1"); _}
                      ; {texpr= FunApp ("exp", [x]); _} ] ); _ } ] ) ->
            FunApp ("log1m_exp", [x])
        | ( "log"
          , [ { texpr=
                  FunApp
                    ( "Minus__"
                    , [ {texpr= Lit (Int, "1"); _}
                      ; {texpr= FunApp ("inv_logit", [x]); _} ] ); _ } ] ) ->
            FunApp ("log1m_inv_logit", [x])
        | ( "log"
          , [{texpr= FunApp ("Minus__", [{texpr= Lit (Int, "1"); _}; x]); _}] )
          ->
            FunApp ("log1m", [x])
        | ( "log"
          , [ { texpr=
                  FunApp
                    ( "Plus__"
                    , [ {texpr= Lit (Int, "1"); _}
                      ; {texpr= FunApp ("exp", [x]); _} ] ); _ } ] ) ->
            FunApp ("log1p_exp", [x])
        | ( "log"
          , [{texpr= FunApp ("Plus__", [{texpr= Lit (Int, "1"); _}; x]); _}] )
          ->
            FunApp ("log1p", [x])
        | ( "log"
          , [ { texpr=
                  FunApp ("fabs", [{texpr= FunApp ("determinant", [x]); _}]); _
              } ] ) ->
            FunApp ("log_determinant", [x])
        (* TODO: can only do below for reals:
    | "log", [FunApp (FunApp ("exp", [x]),"Minus__",FunApp ("exp", [y]))] ->
    FunApp ("log_diff_exp", [x;y]) *)
        (* TODO: log_mix?*)
        | "log", [{texpr= FunApp ("falling_factorial", l); _}] ->
            FunApp ("log_falling_factorial", l)
        | "log", [{texpr= FunApp ("rising_factorial", l); _}] ->
            FunApp ("log_rising_factorial", l)
        | "log", [{texpr= FunApp ("inv_logit", l); _}] ->
            FunApp ("log_inv_logit", l)
        | "log", [{texpr= FunApp ("softmax", l); _}] ->
            FunApp ("log_softmax", l)
        | "log", [{texpr= FunApp ("sum", [{texpr= FunApp ("exp", l); _}]); _}]
          ->
            FunApp ("log_sum_exp", l)
        (* TODO: can only do below for reals:
    | "log", [FunApp (FunApp ("exp", [x]),"Plus__" ,FunApp ("exp", [y]))] ->
    FunApp ("log_sum_exp", [x;y]) *)
        | "multi_normal_lpdf", [y; mu; {texpr= FunApp ("inverse", [tau]); _}]
          ->
            FunApp ("multi_normal_prec_lpdf", [y; mu; tau])
        | "neg_binomial_2_lpmf", [y; {texpr= FunApp ("log", [eta]); _}; phi] ->
            FunApp ("neg_binomial_2_log_lpmf", [y; eta; phi])
        | "neg_binomial_2_rng", [{texpr= FunApp ("log", [eta]); _}; phi] ->
            FunApp ("neg_binomial_2_log_rng", [eta; phi])
        | "poisson_lpmf", [y; {texpr= FunApp ("log", [eta]); _}] ->
            FunApp ("poisson_log_lpmf", [y; eta])
        | "poisson_rng", [{texpr= FunApp ("log", [eta]); _}] ->
            FunApp ("poisson_log_rng", [eta])
        | "pow", [{texpr= Lit (Int, "2"); _}; x] -> FunApp ("exp2", [x])
        | "rows_dot_product", [x; y] when x = y -> FunApp ("rows_dot_self", [x])
        | "pow", [x; {texpr= Lit (Int, "2"); _}] -> FunApp ("square", [x])
        | "pow", [x; {texpr= Lit (Real, "0.5"); _}]
         |( "pow"
          , [ x
            ; { texpr=
                  FunApp
                    ( "Divide__"
                    , [{texpr= Lit (Int, "1"); _}; {texpr= Lit (Int, "2"); _}]
                    ); _ } ] ) ->
            FunApp ("sqrt", [x])
        (* TODO: insert all composite functions here *)
        | "square", [{texpr= FunApp ("sd", [x]); _}] -> FunApp ("variance", [x])
        | "sqrt", [{texpr= Lit (Int, "2"); _}] -> FunApp ("sqrt2", [])
        | ( "sum"
          , [ { texpr=
                  FunApp ("square", [{texpr= FunApp ("Minus__", [x; y]); _}]); _
              } ] ) ->
            FunApp ("squared_distance", [x; y])
        | "sum", [{texpr= FunApp ("diagonal", l); _}] -> FunApp ("trace", l)
        | ( "trace"
          , [ { texpr=
                  FunApp
                    ( "Times__"
                    , [ { texpr=
                            FunApp
                              ( "Times__"
                              , [ { texpr=
                                      FunApp
                                        ( "Times__"
                                        , [ d
                                          ; { texpr= FunApp ("transpose", [b]); _
                                            } ] ); _ }
                                ; a ] ); _ }
                      ; c ] ); _ } ] )
          when b = c ->
            FunApp ("trace_gen_quad_form", [d; a; b])
        | "trace", [{texpr= FunApp ("quad_form", [a; b]); _}] ->
            FunApp ("trace_quad_form", [a; b])
        | op, l -> (
          match (op, l) with
          | ( "Minus__"
            , [{texpr= Lit (Int, "1"); _}; {texpr= FunApp ("erf", l); _}] ) ->
              FunApp ("erfc", l)
          | ( "Minus__"
            , [{texpr= Lit (Int, "1"); _}; {texpr= FunApp ("erfc", l); _}] ) ->
              FunApp ("erf", l)
          | ( "Minus__"
            , [{texpr= FunApp ("exp", l'); _}; {texpr= Lit (Int, "1"); _}] ) ->
              FunApp ("expm1", l')
          (* TODO: can only do below for reals:
    | FunApp (x, "Times__", y), "Plus__", z
    | z, "Plus__", FunApp (x, "Times__",y)-> FunApp ("fma", [x;y;z]) *)
          | ( "Minus__"
            , [{texpr= Lit (Int, "1"); _}; {texpr= FunApp ("gamma_p", l); _}] )
            ->
              FunApp ("gamma_q", l)
          | ( "Minus__"
            , [{texpr= Lit (Int, "1"); _}; {texpr= FunApp ("gamma_q", l); _}] )
            ->
              FunApp ("gamma_p", l)
          (* TODO: can only do below for t reals:       
| FunApp("matrix_exp", [FunApp(t,"Times__" ,a)]), "Times__", b
| FunApp("matrix_exp", [FunApp(a,"Times__" ,t)]), "Times__", b-> FunApp("scale_matrix_exp_multiply", [t;a;b]) *)
          | "Times__", [{texpr= FunApp ("matrix_exp", [a]); _}; b] ->
              FunApp ("matrix_exp_multiply", [a; b])
          (* TODO: can only do below for reals:  
| x, "Times__", FunApp("log", [y]) -> FunApp("multiply_log", [x;y]) *)
          | ( "Times__"
            , [ { texpr=
                    FunApp
                      ("transpose", [{texpr= FunApp ("diag_matrix", [v]); _}]); _
                }
              ; { texpr=
                    FunApp
                      ("Times__", [a; {texpr= FunApp ("diag_matrix", [w]); _}]); _
                } ] )
            when v = w ->
              FunApp ("quad_form_diag", [a; v])
          | ( "Times__"
            , [ { texpr=
                    FunApp
                      ( "Times__"
                      , [ { texpr=
                              FunApp
                                ( "transpose"
                                , [{texpr= FunApp ("diag_matrix", [v]); _}] ); _
                          }
                        ; a ] ); _ }
              ; {texpr= FunApp ("diag_matrix", [w]); _} ] )
            when v = w ->
              FunApp ("quad_form_diag", [a; v])
          | ( "Times__"
            , [ {texpr= FunApp ("transpose", [b]); _}
              ; {texpr= FunApp ("Times__", [a; c]); _} ] )
            when b = c ->
              FunApp ("quad_form", [a; b])
          | ( "Times__"
            , [ { texpr=
                    FunApp
                      ("Times__", [{texpr= FunApp ("transpose", [b]); _}; a]); _
                }
              ; c ] )
            when b = c ->
              FunApp ("quad_form", [a; b])
          | "Times__", [e1'; {texpr= FunApp ("diag_matrix", [v]); _}] ->
              FunApp ("diag_post_multiply", [e1'; v])
          | "Times__", [{texpr= FunApp ("diag_matrix", [v]); _}; e2'] ->
              FunApp ("diag_pre_multiply", [v; e2'])
              (* Constant folding for operators *)
          | op, [{texpr= Lit (Int, i); _}] -> (
            match op with
            | "PPlus_" | "PMinus__" | "PNot__" ->
                apply_prefix_operator_int op (Int.of_string i)
            | _ -> FunApp (op, l) )
          | op, [{texpr= Lit (Real, r); _}] -> (
            match op with
            | "PPlus_" | "PMinus__" ->
                apply_prefix_operator_real op (Float.of_string r)
            | _ -> FunApp (op, l) )
          | op, [{texpr= Lit (Int, i1); _}; {texpr= Lit (Int, i2); _}] -> (
            match op with
            | "Plus__" | "Minus__" | "Times__" | "Divide__" | "Modulo__"
             |"Or__" | "And__" | "Equals__" | "NEquals__" | "Less__"
             |"Leq__" | "Greater__" | "Geq__" ->
                apply_operator_int op (Int.of_string i1) (Int.of_string i2)
            | _ -> FunApp (op, l) )
          | op, [{texpr= Lit (Real, i1); _}; {texpr= Lit (Real, i2); _}]
           |op, [{texpr= Lit (Int, i1); _}; {texpr= Lit (Real, i2); _}]
           |op, [{texpr= Lit (Real, i1); _}; {texpr= Lit (Int, i2); _}] -> (
            match op with
            | "Plus__" | "Minus__" | "Times__" | "Divide__" ->
                apply_arithmetic_operator_real op (Float.of_string i1)
                  (Float.of_string i2)
            | "Or__" | "And__" | "Equals__" | "NEquals__" | "Less__"
             |"Leq__" | "Greater__" | "Geq__" ->
                apply_logical_operator_real op (Float.of_string i1)
                  (Float.of_string i2)
            | _ -> FunApp (op, l) )
          | _ -> FunApp (op, l) ) )
      | TernaryIf (e1, e2, e3) -> (
        match (eval_expr e1, eval_expr e2, eval_expr e3) with
        | {texpr= Lit (Int, "0"); _}, _, e3' -> e3'.texpr
        | {texpr= Lit (Int, _); _}, e2', _ -> e2'.texpr
        | e1', e2', e3' -> TernaryIf (e1', e2', e3') )
      | Indexed (e, l) ->
          (* TODO: do something clever with array and matrix expressions here?
  Note  that we could also constant fold array sizes if we keep those around on declarations. *)
          Indexed (eval_expr e, List.map ~f:eval_idx l) ) }

and eval_idx i = map_index eval_expr i

let eval_stmt_base = map_statement eval_expr (fun x -> x)
let eval_stmt = map_rec_stmt_loc eval_stmt_base
let eval_prog = map_prog eval_expr eval_stmt
