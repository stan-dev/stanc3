(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel
open Mir_utils
open Middle

let preserve_stability = false

let is_int i Expr.Fixed.({pattern; _}) =
  let nums = List.map ~f:(fun s -> string_of_int i ^ s) [""; "."; ".0"] in
  match pattern with
  | (Lit (Int, i) | Lit (Real, i)) when List.mem nums i ~equal:String.equal ->
      true
  | _ -> false

let apply_prefix_operator_int (op : string) i =
  Expr.Fixed.Pattern.Lit
    ( Int
    , Int.to_string
        ( match op with
        | "PPlus__" -> i
        | "PMinus__" -> -i
        | "PNot__" -> if i = 0 then 1 else 0
        | s -> raise_s [%sexp (s : string)] ) )

let apply_prefix_operator_real (op : string) i =
  Expr.Fixed.Pattern.Lit
    ( Real
    , Float.to_string
        ( match op with
        | "PPlus__" -> i
        | "PMinus__" -> -.i
        | s -> raise_s [%sexp (s : string)] ) )

let apply_operator_int (op : string) i1 i2 =
  Expr.Fixed.Pattern.Lit
    ( Int
    , Int.to_string
        ( match op with
        | "Plus__" -> i1 + i2
        | "Minus__" -> i1 - i2
        | "Times__" -> i1 * i2
        | "Divide__" -> i1 / i2
        | "IntDivide__" -> i1 / i2
        | "Modulo__" -> i1 % i2
        | "Equals__" -> Bool.to_int (i1 = i2)
        | "NEquals__" -> Bool.to_int (i1 <> i2)
        | "Less__" -> Bool.to_int (i1 < i2)
        | "Leq__" -> Bool.to_int (i1 <= i2)
        | "Greater__" -> Bool.to_int (i1 > i2)
        | "Geq__" -> Bool.to_int (i1 >= i2)
        | s -> raise_s [%sexp (s : string)] ) )

let apply_arithmetic_operator_real (op : string) r1 r2 =
  Expr.Fixed.Pattern.Lit
    ( Real
    , Float.to_string
        ( match op with
        | "Plus__" -> r1 +. r2
        | "Minus__" -> r1 -. r2
        | "Times__" -> r1 *. r2
        | "Divide__" -> r1 /. r2
        | s -> raise_s [%sexp (s : string)] ) )

let apply_logical_operator_real (op : string) r1 r2 =
  Expr.Fixed.Pattern.Lit
    ( Int
    , Int.to_string
        ( match op with
        | "Equals__" -> Bool.to_int (r1 = r2)
        | "NEquals__" -> Bool.to_int (r1 <> r2)
        | "Less__" -> Bool.to_int (r1 < r2)
        | "Leq__" -> Bool.to_int (r1 <= r2)
        | "Greater__" -> Bool.to_int (r1 > r2)
        | "Geq__" -> Bool.to_int (r1 >= r2)
        | s -> raise_s [%sexp (s : string)] ) )

let is_multi_index = function
  | Index.MultiIndex _ | Upfrom _ | Between _ | All -> true
  | Single _ -> false

let rec eval_expr (e : Expr.Typed.t) =
  { e with
    pattern=
      ( match e.pattern with
      | Var _ | Lit (_, _) -> e.pattern
      | FunApp (kind, l) -> (
          let l = List.map ~f:eval_expr l in
          match kind with
          | UserDefined _ | CompilerInternal _ -> FunApp (kind, l)
          | StanLib f ->
              let get_fun_or_op_rt_opt name l' =
                let argument_types =
                  List.map
                    ~f:(fun x -> Expr.Typed.(adlevel_of x, type_of x))
                    l'
                in
                Operator.of_string_opt name
                |> Option.value_map
                     ~f:(fun op ->
                       Stan_math_signatures.operator_stan_math_return_type op
                         argument_types )
                     ~default:
                       (Stan_math_signatures.stan_math_returntype name
                          argument_types)
              in
              let try_partially_evaluate_stanlib e =
                Expr.Fixed.Pattern.(
                  match e with
                  | FunApp (StanLib f', l') -> (
                    match get_fun_or_op_rt_opt f' l' with
                    | Some _ -> FunApp (StanLib f', l')
                    | None -> FunApp (StanLib f, l) )
                  | e -> e)
              in
              try_partially_evaluate_stanlib
                ( match (f, l) with
                (* TODO: deal with tilde statements and unnormalized distributions properly here *)
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "inv_logit"
                            , [ { pattern=
                                    FunApp
                                      ( StanLib "Plus__"
                                      , [ alpha
                                        ; { pattern=
                                              FunApp
                                                (StanLib "Times__", [x; beta]); _
                                          } ] ); _ } ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      (StanLib "bernoulli_logit_glm_lpmf", [y; x; alpha; beta])
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "inv_logit"
                            , [ { pattern=
                                    FunApp
                                      ( StanLib "Plus__"
                                      , [ { pattern=
                                              FunApp
                                                (StanLib "Times__", [x; beta]); _
                                          }
                                        ; alpha ] ); _ } ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      (StanLib "bernoulli_logit_glm_lpmf", [y; x; alpha; beta])
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "inv_logit"
                            , [ { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                } ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "bernoulli_logit_glm_lpmf"
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "bernoulli_logit_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ alpha
                              ; { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                } ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      (StanLib "bernoulli_logit_glm_lpmf", [y; x; alpha; beta])
                | ( "bernoulli_logit_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                }
                              ; alpha ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      (StanLib "bernoulli_logit_glm_lpmf", [y; x; alpha; beta])
                | ( "bernoulli_logit_lpmf"
                  , [y; {pattern= FunApp (StanLib "Times__", [x; beta]); _}] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "bernoulli_logit_glm_lpmf"
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "bernoulli_lpmf"
                  , [y; {pattern= FunApp (StanLib "inv_logit", [alpha]); _}] )
                  ->
                    FunApp (StanLib "bernoulli_logit_lpmf", [y; alpha])
                | ( "bernoulli_rng"
                  , [{pattern= FunApp (StanLib "inv_logit", [alpha]); _}] ) ->
                    FunApp (StanLib "bernoulli_logit_rng", [alpha])
                | ( "binomial_lpmf"
                  , [y; n; {pattern= FunApp (StanLib "inv_logit", [alpha]); _}]
                  ) ->
                    FunApp (StanLib "binomial_logit_lpmf", [y; n; alpha])
                | ( "categorical_lpmf"
                  , [y; {pattern= FunApp (StanLib "inv_logit", [alpha]); _}] )
                  ->
                    FunApp (StanLib "categorical_logit_lpmf", [y; alpha])
                | ( "categorical_rng"
                  , [{pattern= FunApp (StanLib "inv_logit", [alpha]); _}] ) ->
                    FunApp (StanLib "categorical_logit_rng", [alpha])
                | "columns_dot_product", [x; y] when Expr.Typed.equal x y ->
                    FunApp (StanLib "columns_dot_self", [x])
                | "dot_product", [x; y] when Expr.Typed.equal x y ->
                    FunApp (StanLib "dot_self", [x])
                | "inv", [{pattern= FunApp (StanLib "sqrt", l); _}] ->
                    FunApp (StanLib "inv_sqrt", l)
                | "inv", [{pattern= FunApp (StanLib "square", [x]); _}] ->
                    FunApp (StanLib "inv_square", [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib "Minus__"
                            , [y; {pattern= FunApp (StanLib "exp", [x]); _}] ); _
                      } ] )
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib "log1m_exp", [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib "Minus__"
                            , [ y
                              ; {pattern= FunApp (StanLib "inv_logit", [x]); _}
                              ] ); _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib "log1m_inv_logit", [x])
                | "log", [{pattern= FunApp (StanLib "Minus__", [y; x]); _}]
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib "log1m", [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [y; {pattern= FunApp (StanLib "exp", [x]); _}] ); _
                      } ] )
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib "log1p_exp", [x])
                | "log", [{pattern= FunApp (StanLib "Plus__", [y; x]); _}]
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib "log1p", [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib "fabs"
                            , [ { pattern= FunApp (StanLib "determinant", [x]); _
                                } ] ); _ } ] ) ->
                    FunApp (StanLib "log_determinant", [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib "Minus__"
                            , [ {pattern= FunApp (StanLib "exp", [x]); _}
                              ; {pattern= FunApp (StanLib "exp", [y]); _} ] ); _
                      } ] ) ->
                    FunApp (StanLib "log_diff_exp", [x; y])
                (* TODO: log_mix?*)
                | "log", [{pattern= FunApp (StanLib "falling_factorial", l); _}]
                  ->
                    FunApp (StanLib "log_falling_factorial", l)
                | "log", [{pattern= FunApp (StanLib "rising_factorial", l); _}]
                  ->
                    FunApp (StanLib "log_rising_factorial", l)
                | "log", [{pattern= FunApp (StanLib "inv_logit", l); _}] ->
                    FunApp (StanLib "log_inv_logit", l)
                | "log", [{pattern= FunApp (StanLib "softmax", l); _}] ->
                    FunApp (StanLib "log_softmax", l)
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib "sum"
                            , [{pattern= FunApp (StanLib "exp", l); _}] ); _ }
                    ] ) ->
                    FunApp (StanLib "log_sum_exp", l)
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ {pattern= FunApp (StanLib "exp", [x]); _}
                              ; {pattern= FunApp (StanLib "exp", [y]); _} ] ); _
                      } ] ) ->
                    FunApp (StanLib "log_sum_exp", [x; y])
                | ( "multi_normal_lpdf"
                  , [y; mu; {pattern= FunApp (StanLib "inverse", [tau]); _}] )
                  ->
                    FunApp (StanLib "multi_normal_prec_lpdf", [y; mu; tau])
                | ( "neg_binomial_2_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "exp"
                            , [ { pattern=
                                    FunApp
                                      ( StanLib "Plus__"
                                      , [ alpha
                                        ; { pattern=
                                              FunApp
                                                (StanLib "Times__", [x; beta]); _
                                          } ] ); _ } ] ); _ }
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "neg_binomial_2_log_glm_lpmf"
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "exp"
                            , [ { pattern=
                                    FunApp
                                      ( StanLib "Plus__"
                                      , [ { pattern=
                                              FunApp
                                                (StanLib "Times__", [x; beta]); _
                                          }
                                        ; alpha ] ); _ } ] ); _ }
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "neg_binomial_2_log_glm_lpmf"
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "exp"
                            , [ { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                } ] ); _ }
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "neg_binomial_2_log_glm_lpmf"
                      , [y; x; Expr.Helpers.zero; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ alpha
                              ; { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                } ] ); _ }
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "neg_binomial_2_log_glm_lpmf"
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                }
                              ; alpha ] ); _ }
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "neg_binomial_2_log_glm_lpmf"
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [ y
                    ; {pattern= FunApp (StanLib "Times__", [x; beta]); _}
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "neg_binomial_2_log_glm_lpmf"
                      , [y; x; Expr.Helpers.zero; beta; sigma] )
                | ( "neg_binomial_2_lpmf"
                  , [y; {pattern= FunApp (StanLib "exp", [eta]); _}; phi] ) ->
                    FunApp (StanLib "neg_binomial_2_log_lpmf", [y; eta; phi])
                | ( "neg_binomial_2_rng"
                  , [{pattern= FunApp (StanLib "exp", [eta]); _}; phi] ) ->
                    FunApp (StanLib "neg_binomial_2_log_rng", [eta; phi])
                | ( "normal_lpdf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ alpha
                              ; { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                } ] ); _ }
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      (StanLib "normal_id_glm_lpdf", [y; x; alpha; beta; sigma])
                | ( "normal_lpdf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                }
                              ; alpha ] ); _ }
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      (StanLib "normal_id_glm_lpdf", [y; x; alpha; beta; sigma])
                | ( "normal_lpdf"
                  , [ y
                    ; {pattern= FunApp (StanLib "Times__", [x; beta]); _}
                    ; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "normal_id_glm_lpdf"
                      , [y; x; Expr.Helpers.zero; beta; sigma] )
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "exp"
                            , [ { pattern=
                                    FunApp
                                      ( StanLib "Plus__"
                                      , [ alpha
                                        ; { pattern=
                                              FunApp
                                                (StanLib "Times__", [x; beta]); _
                                          } ] ); _ } ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp (StanLib "poisson_log_glm_lpmf", [y; x; alpha; beta])
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "exp"
                            , [ { pattern=
                                    FunApp
                                      ( StanLib "Plus__"
                                      , [ { pattern=
                                              FunApp
                                                (StanLib "Times__", [x; beta]); _
                                          }
                                        ; alpha ] ); _ } ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp (StanLib "poisson_log_glm_lpmf", [y; x; alpha; beta])
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "exp"
                            , [ { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                } ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "poisson_log_glm_lpmf"
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "poisson_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ alpha
                              ; { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                } ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp (StanLib "poisson_log_glm_lpmf", [y; x; alpha; beta])
                | ( "poisson_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib "Plus__"
                            , [ { pattern= FunApp (StanLib "Times__", [x; beta]); _
                                }
                              ; alpha ] ); _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp (StanLib "poisson_log_glm_lpmf", [y; x; alpha; beta])
                | ( "poisson_log_lpmf"
                  , [y; {pattern= FunApp (StanLib "Times__", [x; beta]); _}] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib "poisson_log_glm_lpmf"
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "poisson_lpmf"
                  , [y; {pattern= FunApp (StanLib "exp", [eta]); _}] ) ->
                    FunApp (StanLib "poisson_log_lpmf", [y; eta])
                | "poisson_rng", [{pattern= FunApp (StanLib "exp", [eta]); _}]
                  ->
                    FunApp (StanLib "poisson_log_rng", [eta])
                | "pow", [y; x] when is_int 2 y -> FunApp (StanLib "exp2", [x])
                | "rows_dot_product", [x; y] when Expr.Typed.equal x y ->
                    FunApp (StanLib "rows_dot_self", [x])
                | "pow", [x; {pattern= Lit (Int, "2"); _}] ->
                    FunApp (StanLib "square", [x])
                | "pow", [x; {pattern= Lit (Real, "0.5"); _}] ->
                    FunApp (StanLib "sqrt", [x])
                | "pow", [x; {pattern= FunApp (StanLib "Divide__", [y; z]); _}]
                  when is_int 1 y && is_int 2 z ->
                    FunApp (StanLib "sqrt", [x])
                    (* This is wrong; if both are type UInt the exponent is rounds down to zero. *)
                | "square", [{pattern= FunApp (StanLib "sd", [x]); _}] ->
                    FunApp (StanLib "variance", [x])
                | "sqrt", [x] when is_int 2 x -> FunApp (StanLib "sqrt2", [])
                | ( "sum"
                  , [ { pattern=
                          FunApp
                            ( StanLib "square"
                            , [{pattern= FunApp (StanLib "Minus__", [x; y]); _}]
                            ); _ } ] ) ->
                    FunApp (StanLib "squared_distance", [x; y])
                | "sum", [{pattern= FunApp (StanLib "diagonal", l); _}] ->
                    FunApp (StanLib "trace", l)
                | ( "trace"
                  , [ { pattern=
                          FunApp
                            ( StanLib "Times__"
                            , [ { pattern=
                                    FunApp
                                      ( StanLib "Times__"
                                      , [ { pattern=
                                              FunApp
                                                ( StanLib "Times__"
                                                , [ d
                                                  ; { pattern=
                                                        FunApp
                                                          ( StanLib "transpose"
                                                          , [b] ); _ } ] ); _
                                          }
                                        ; a ] ); _ }
                              ; c ] ); _ } ] )
                  when Expr.Typed.equal b c ->
                    FunApp (StanLib "trace_gen_quad_form", [d; a; b])
                | "trace", [{pattern= FunApp (StanLib "quad_form", [a; b]); _}]
                  ->
                    FunApp (StanLib "trace_quad_form", [a; b])
                | "Minus__", [x; {pattern= FunApp (StanLib "erf", l); _}]
                  when is_int 1 x ->
                    FunApp (StanLib "erfc", l)
                | "Minus__", [x; {pattern= FunApp (StanLib "erfc", l); _}]
                  when is_int 1 x ->
                    FunApp (StanLib "erf", l)
                | "Minus__", [{pattern= FunApp (StanLib "exp", l'); _}; x]
                  when is_int 1 x && not preserve_stability ->
                    FunApp (StanLib "expm1", l')
                | ( "Plus__"
                  , [{pattern= FunApp (StanLib "Times__", [x; y]); _}; z] )
                 |( "Plus__"
                  , [z; {pattern= FunApp (StanLib "Times__", [x; y]); _}] )
                  when not preserve_stability ->
                    FunApp (StanLib "fma", [x; y; z])
                | "Minus__", [x; {pattern= FunApp (StanLib "gamma_p", l); _}]
                  when is_int 1 x ->
                    FunApp (StanLib "gamma_q", l)
                | "Minus__", [x; {pattern= FunApp (StanLib "gamma_q", l); _}]
                  when is_int 1 x ->
                    FunApp (StanLib "gamma_p", l)
                | ( "Times__"
                  , [ { pattern=
                          FunApp
                            ( StanLib "matrix_exp"
                            , [{pattern= FunApp (StanLib "Times__", [t; a]); _}]
                            ); _ }
                    ; b ] )
                  when Expr.Typed.type_of t = UInt
                       || Expr.Typed.type_of t = UReal ->
                    FunApp (StanLib "scale_matrix_exp_multiply", [t; a; b])
                | ( "Times__"
                  , [ { pattern=
                          FunApp
                            ( StanLib "matrix_exp"
                            , [{pattern= FunApp (StanLib "Times__", [a; t]); _}]
                            ); _ }
                    ; b ] )
                  when Expr.Typed.type_of t = UInt
                       || Expr.Typed.type_of t = UReal ->
                    FunApp (StanLib "scale_matrix_exp_multiply", [t; a; b])
                | ( "Times__"
                  , [{pattern= FunApp (StanLib "matrix_exp", [a]); _}; b] ) ->
                    FunApp (StanLib "matrix_exp_multiply", [a; b])
                | "Times__", [x; {pattern= FunApp (StanLib "log", [y]); _}]
                 |"Times__", [{pattern= FunApp (StanLib "log", [y]); _}; x]
                  when not preserve_stability ->
                    FunApp (StanLib "lmultiply", [x; y])
                | ( "Times__"
                  , [ {pattern= FunApp (StanLib "diag_matrix", [v]); _}
                    ; { pattern= FunApp (StanLib "diag_post_multiply", [a; w]); _
                      } ] )
                  when Expr.Typed.equal v w ->
                    FunApp (StanLib "quad_form_diag", [a; v])
                | ( "Times__"
                  , [ {pattern= FunApp (StanLib "diag_pre_multiply", [v; a]); _}
                    ; {pattern= FunApp (StanLib "diag_matrix", [w]); _} ] )
                  when Expr.Typed.equal v w ->
                    FunApp (StanLib "quad_form_diag", [a; v])
                | ( "Times__"
                  , [ {pattern= FunApp (StanLib "transpose", [b]); _}
                    ; {pattern= FunApp (StanLib "Times__", [a; c]); _} ] )
                  when Expr.Typed.equal b c ->
                    FunApp (StanLib "quad_form", [a; b])
                | ( "Times__"
                  , [ { pattern=
                          FunApp
                            ( StanLib "Times__"
                            , [ {pattern= FunApp (StanLib "transpose", [b]); _}
                              ; a ] ); _ }
                    ; c ] )
                  when Expr.Typed.equal b c ->
                    FunApp (StanLib "quad_form", [a; b])
                | ( "Times__"
                  , [e1'; {pattern= FunApp (StanLib "diag_matrix", [v]); _}] )
                  ->
                    FunApp (StanLib "diag_post_multiply", [e1'; v])
                | ( "Times__"
                  , [{pattern= FunApp (StanLib "diag_matrix", [v]); _}; e2'] )
                  ->
                    FunApp (StanLib "diag_pre_multiply", [v; e2'])
                    (* Constant folding for operators *)
                | op, [{pattern= Lit (Int, i); _}] -> (
                  match op with
                  | "PPlus__" | "PMinus__" | "PNot__" ->
                      apply_prefix_operator_int op (Int.of_string i)
                  | _ -> FunApp (kind, l) )
                | op, [{pattern= Lit (Real, r); _}] -> (
                  match op with
                  | "PPlus__" | "PMinus__" ->
                      apply_prefix_operator_real op (Float.of_string r)
                  | _ -> FunApp (kind, l) )
                | op, [{pattern= Lit (Int, i1); _}; {pattern= Lit (Int, i2); _}]
                -> (
                  match op with
                  | "Plus__" | "Minus__" | "Times__" | "Divide__"
                   |"IntDivide__" | "Modulo__" | "Or__" | "And__"
                   |"Equals__" | "NEquals__" | "Less__" | "Leq__"
                   |"Greater__" | "Geq__" ->
                      apply_operator_int op (Int.of_string i1)
                        (Int.of_string i2)
                  | _ -> FunApp (kind, l) )
                | ( op
                  , [{pattern= Lit (Real, i1); _}; {pattern= Lit (Real, i2); _}]
                  )
                 |( op
                  , [{pattern= Lit (Int, i1); _}; {pattern= Lit (Real, i2); _}]
                  )
                 |( op
                  , [{pattern= Lit (Real, i1); _}; {pattern= Lit (Int, i2); _}]
                  ) -> (
                  match op with
                  | "Plus__" | "Minus__" | "Times__" | "Divide__" ->
                      apply_arithmetic_operator_real op (Float.of_string i1)
                        (Float.of_string i2)
                  | "Or__" | "And__" | "Equals__" | "NEquals__" | "Less__"
                   |"Leq__" | "Greater__" | "Geq__" ->
                      apply_logical_operator_real op (Float.of_string i1)
                        (Float.of_string i2)
                  | _ -> FunApp (kind, l) )
                | _ -> FunApp (kind, l) ) )
      | TernaryIf (e1, e2, e3) -> (
        match (eval_expr e1, eval_expr e2, eval_expr e3) with
        | x, _, e3' when is_int 0 x -> e3'.pattern
        | {pattern= Lit (Int, _); _}, e2', _ -> e2'.pattern
        | e1', e2', e3' -> TernaryIf (e1', e2', e3') )
      | EAnd (e1, e2) -> (
        match (eval_expr e1, eval_expr e2) with
        | {pattern= Lit (Int, s1); _}, {pattern= Lit (Int, s2); _} ->
            let i1, i2 = (Int.of_string s1, Int.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (i1 <> 0 && i2 <> 0)))
        | {pattern= Lit (_, s1); _}, {pattern= Lit (_, s2); _} ->
            let r1, r2 = (Float.of_string s1, Float.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (r1 <> 0. && r2 <> 0.)))
        | e1', e2' -> EAnd (e1', e2') )
      | EOr (e1, e2) -> (
        match (eval_expr e1, eval_expr e2) with
        | {pattern= Lit (Int, s1); _}, {pattern= Lit (Int, s2); _} ->
            let i1, i2 = (Int.of_string s1, Int.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (i1 <> 0 || i2 <> 0)))
        | {pattern= Lit (_, s1); _}, {pattern= Lit (_, s2); _} ->
            let r1, r2 = (Float.of_string s1, Float.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (r1 <> 0. || r2 <> 0.)))
        | e1', e2' -> EOr (e1', e2') )
      | Indexed (e, l) ->
          (* TODO: do something clever with array and matrix expressions here?
       Note  that we could also constant fold array sizes if we keep those around on declarations. *)
          Indexed (eval_expr e, List.map ~f:(Index.map eval_expr) l) ) }

let rec simplify_index_expr pattern =
  Expr.Fixed.(
    match pattern with
    | Pattern.Indexed
        ( { pattern=
              Indexed (obj, inner_indices)
              (* , Single ({emeta= {type_= UArray UInt; _} as emeta; _} as multi)
               *   :: inner_tl ) *)
          ; meta }
        , ( Single ({meta= Expr.Typed.Meta.({type_= UInt; _}); _} as single_e)
          as single )
          :: outer_tl )
      when List.exists ~f:is_multi_index inner_indices -> (
      match List.split_while ~f:(Fn.non is_multi_index) inner_indices with
      | inner_singles, MultiIndex first_multi :: inner_tl ->
          (* foo [arr1, ..., arrN] [i1, ..., iN] ->
         foo [arr1[i1]] [arr[i2]] ... [arrN[iN]] *)
          simplify_index_expr
            (Indexed
               ( { pattern=
                     Indexed
                       ( obj
                       , inner_singles
                         @ [ Index.Single
                               { pattern= Indexed (first_multi, [single])
                               ; meta= {meta with type_= UInt} } ]
                         @ inner_tl )
                 ; meta }
               , outer_tl ))
      | inner_singles, All :: inner_tl ->
          (* v[:x][i] -> v[i] *)
          (* v[:][i] -> v[i] *)
          (* XXX generate check *)
          simplify_index_expr
            (Indexed
               ( { pattern= Indexed (obj, inner_singles @ [single] @ inner_tl)
                 ; meta }
               , outer_tl ))
      | inner_singles, Between (bot, _) :: inner_tl
       |inner_singles, Upfrom bot :: inner_tl ->
          (* v[x:y][z] -> v[x+z-1] *)
          (* XXX generate check *)
          simplify_index_expr
            (Indexed
               ( { pattern=
                     Indexed
                       ( obj
                       , inner_singles
                         @ [ Index.Single
                               Expr.Helpers.(
                                 binop (binop bot Plus single_e) Minus
                                   loop_bottom) ]
                         @ inner_tl )
                 ; meta }
               , outer_tl ))
      | inner_singles, (([] | Single _ :: _) as multis) ->
          raise_s
            [%message
              "Impossible! There must be a multi-index."
                (inner_singles : Expr.Typed.t Index.t list)
                (multis : Expr.Typed.t Index.t list)] )
    | e -> e)

let remove_trailing_alls_expr = function
  | Expr.Fixed.Pattern.Indexed (obj, indices) ->
      (* a[2][:] -> a[2] *)
      let rec remove_trailing_alls indices =
        match List.rev indices with
        | Index.All :: tl -> remove_trailing_alls (List.rev tl)
        | _ -> indices
      in
      Expr.Fixed.Pattern.Indexed (obj, remove_trailing_alls indices)
  | e -> e

let rec simplify_indices_expr expr =
  Expr.Fixed.(
    let pattern =
      expr.pattern |> remove_trailing_alls_expr |> simplify_index_expr
      |> Expr.Fixed.Pattern.map simplify_indices_expr
    in
    {expr with pattern})

let eval_stmt_base =
  Stmt.Fixed.Pattern.map (Fn.compose eval_expr simplify_indices_expr) Fn.id

let eval_stmt = map_rec_stmt_loc eval_stmt_base
let eval_prog = Program.map eval_expr eval_stmt
