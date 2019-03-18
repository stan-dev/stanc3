(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel
open Mir

let rec subst_expr (m : (string, expr_typed_located) Map.Poly.t)
    (e : expr_typed_located) =
  match e.texpr with
  | Var s -> ( match Map.find m s with Some e' -> e' | None -> e )
  | Lit (_, _) -> e
  | FunApp (f, l) -> {e with texpr= FunApp (f, List.map ~f:(subst_expr m) l)}
  | TernaryIf (e1, e2, e3) ->
      { e with
        texpr= TernaryIf (subst_expr m e1, subst_expr m e2, subst_expr m e3) }
  | Indexed (e, l) ->
      {e with texpr= Indexed (subst_expr m e, List.map ~f:(subst_idx m) l)}

and subst_idx m i =
  match i with
  | All -> All
  | Single e -> Single (subst_expr m e)
  | Upfrom e -> Upfrom (subst_expr m e)
  | Downfrom e -> Downfrom (subst_expr m e)
  | Between (e1, e2) -> Between (subst_expr m e1, subst_expr m e2)
  | MultiIndex e -> MultiIndex (subst_expr m e)

let subst_stmt_base m b =
  let f = subst_expr m in
  match b with
  | Assignment ({texpr= Var x; texpr_type; texpr_loc; texpr_adlevel}, e2) ->
      Assignment ({texpr= Var x; texpr_type; texpr_loc; texpr_adlevel}, f e2)
  | Assignment
      ( { texpr=
            Indexed
              ( {texpr= Var x; texpr_type= t2; texpr_loc= l2; texpr_adlevel= a2}
              , l )
        ; texpr_type
        ; texpr_loc
        ; texpr_adlevel }
      , e2 ) ->
      Assignment
        ( { texpr=
              Indexed
                ( { texpr= Var x
                  ; texpr_type= t2
                  ; texpr_loc= l2
                  ; texpr_adlevel= a2 }
                , List.map ~f:(subst_idx m) l )
          ; texpr_type
          ; texpr_loc
          ; texpr_adlevel }
        , f e2 )
  | TargetPE e -> TargetPE (f e)
  | NRFunApp (s, e_list) -> NRFunApp (s, List.map e_list ~f)
  | Check (ccfunname, ccargs) -> Check (ccfunname, List.map ccargs ~f)
  | Return opt_e -> Return (Option.map opt_e ~f)
  | IfElse (e, b1, b2) -> IfElse (f e, b1, b2)
  | While (e, b) -> While (f e, b)
  | For {loopvar; lower; upper; body} ->
      For {loopvar; lower= f lower; upper= f upper; body}
  | Block sl -> Block sl
  | SList sl -> SList sl
  | FunDef {fdrt; fdname; fdargs; fdbody} ->
      FunDef {fdrt; fdname; fdargs; fdbody}
  | x -> x

let subst_stmt m = Mir.map_rec_stmt_loc (subst_stmt_base m)

(* TODO: parameterize statement also over expressions and then define the above with a
   recursive map *)

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

let rec eval (e : Mir.expr_typed_located) =
  { e with
    texpr=
      ( match e.texpr with
      | Var _ | Lit (_, _) -> e.texpr
      | FunApp (f, l) -> (
        match (f, List.map ~f:eval l) with
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
        | op, [e1; e2] -> (
          match (e1, op, e2) with
          | {texpr= Lit (Int, "1"); _}, "Minus__", {texpr= FunApp ("erf", l); _}
            ->
              FunApp ("erfc", l)
          | ( {texpr= Lit (Int, "1"); _}
            , "Minus__"
            , {texpr= FunApp ("erfc", l); _} ) ->
              FunApp ("erf", l)
          | ( {texpr= FunApp ("exp", l'); _}
            , "Minus__"
            , {texpr= Lit (Int, "1"); _} ) ->
              FunApp ("expm1", l')
          (* TODO: can only do below for reals:
    | FunApp (x, "Times__", y), "Plus__", z
    | z, "Plus__", FunApp (x, "Times__",y)-> FunApp ("fma", [x;y;z]) *)
          | ( {texpr= Lit (Int, "1"); _}
            , "Minus__"
            , {texpr= FunApp ("gamma_p", l); _} ) ->
              FunApp ("gamma_q", l)
          | ( {texpr= Lit (Int, "1"); _}
            , "Minus__"
            , {texpr= FunApp ("gamma_q", l); _} ) ->
              FunApp ("gamma_p", l)
          (* TODO: can only do below for t reals:       
| FunApp("matrix_exp", [FunApp(t,"Times__" ,a)]), "Times__", b
| FunApp("matrix_exp", [FunApp(a,"Times__" ,t)]), "Times__", b-> FunApp("scale_matrix_exp_multiply", [t;a;b]) *)
          | {texpr= FunApp ("matrix_exp", [a]); _}, "Times__", b ->
              FunApp ("matrix_exp_multiply", [a; b])
          (* TODO: can only do below for reals:  
| x, "Times__", FunApp("log", [y]) -> FunApp("multiply_log", [x;y]) *)
          | ( { texpr=
                  FunApp
                    ("transpose", [{texpr= FunApp ("diag_matrix", [v]); _}]); _
              }
            , "Times__"
            , { texpr=
                  FunApp
                    ("Times__", [a; {texpr= FunApp ("diag_matrix", [w]); _}]); _
              } )
            when v = w ->
              FunApp ("quad_form_diag", [a; v])
          | ( { texpr=
                  FunApp
                    ( "Times__"
                    , [ { texpr=
                            FunApp
                              ( "transpose"
                              , [{texpr= FunApp ("diag_matrix", [v]); _}] ); _
                        }
                      ; a ] ); _ }
            , "Times__"
            , {texpr= FunApp ("diag_matrix", [w]); _} )
            when v = w ->
              FunApp ("quad_form_diag", [a; v])
          | ( {texpr= FunApp ("transpose", [b]); _}
            , "Times__"
            , {texpr= FunApp ("Times__", [a; c]); _} )
            when b = c ->
              FunApp ("quad_form", [a; b])
          | ( { texpr=
                  FunApp ("Times__", [{texpr= FunApp ("transpose", [b]); _}; a]); _
              }
            , "Times__"
            , c )
            when b = c ->
              FunApp ("quad_form", [a; b])
          | e1', "Times__", {texpr= FunApp ("diag_matrix", [v]); _} ->
              FunApp ("diag_post_multiply", [e1'; v])
          | {texpr= FunApp ("diag_matrix", [v]); _}, "Times__", e2' ->
              FunApp ("diag_pre_multiply", [v; e2'])
              (* Constant folding for operators *)
          | {texpr= Lit (Int, i1); _}, _, {texpr= Lit (Int, i2); _} ->
              apply_operator_int op (Int.of_string i1) (Int.of_string i2)
          | {texpr= Lit (Real, i1); _}, _, {texpr= Lit (Real, i2); _}
           |{texpr= Lit (Int, i1); _}, _, {texpr= Lit (Real, i2); _}
           |{texpr= Lit (Real, i1); _}, _, {texpr= Lit (Int, i2); _} -> (
            match op with
            | "Plus__" | "Minus__" | "Times__" | "Divide__" ->
                apply_arithmetic_operator_real op (Float.of_string i1)
                  (Float.of_string i2)
            | "Or__" | "And__" | "Equals__" | "NEquals__" | "Less__"
             |"Leq__" | "Greater__" | "Geq__" ->
                apply_logical_operator_real op (Float.of_string i1)
                  (Float.of_string i2)
            | _ -> FunApp (op, [e1; e2]) )
          | _ -> FunApp (op, [e1; e2]) )
        | _, l' -> FunApp (f, l') )
      | TernaryIf (e1, e2, e3) -> (
        match (eval e1, eval e2, eval e3) with
        | {texpr= Lit (Int, "0"); _}, _, e3' -> e3'.texpr
        | {texpr= Lit (Int, _); _}, e2', _ -> e2'.texpr
        | e1', e2', e3' -> TernaryIf (e1', e2', e3') )
      | Indexed (e, l) ->
          (* TODO: do something clever with array and matrix expressions here?
  Note  that we could also constant fold array sizes if we keep those around on declarations. *)
          Indexed (eval e, List.map ~f:eval_idx l) ) }

and eval_idx i =
  match i with
  | All -> All
  | Single e -> Single (eval e)
  | Upfrom e -> Upfrom (eval e)
  | Downfrom e -> Downfrom (eval e)
  | Between (e1, e2) -> Between (eval e1, eval e2)
  | MultiIndex e -> MultiIndex (eval e)

let eval_subst (m : (string, expr_typed_located) Map.Poly.t)
    (e : expr_typed_located) =
  eval (subst_expr m e)
