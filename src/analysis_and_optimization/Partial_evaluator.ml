(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel
open Core_kernel.Poly
open Middle

exception Rejected of Location_span.t * string

let rec is_int query Expr.Fixed.{pattern; _} =
  match pattern with
  | Lit (Int, i) | Lit (Real, i) -> float_of_string i = float_of_int query
  | Promotion (e, _, _) -> is_int query e
  | _ -> false

let apply_prefix_operator_int (op : string) i =
  Expr.Fixed.Pattern.Lit
    ( Int
    , Int.to_string
        ( match op with
        | "PPlus__" -> i
        | "PMinus__" -> -i
        | "PNot__" -> if i = 0 then 1 else 0
        | s ->
            Common.FatalError.fatal_error_msg
              [%message "Not an int prefix operator: " s] ) )

let apply_prefix_operator_real (op : string) i =
  Expr.Fixed.Pattern.Lit
    ( Real
    , Float.to_string
        ( match op with
        | "PPlus__" -> i
        | "PMinus__" -> -.i
        | s ->
            Common.FatalError.fatal_error_msg
              [%message "Not a real prefix operator: " s] ) )

let apply_operator_int (op : string) i1 i2 =
  Expr.Fixed.Pattern.Lit
    ( Int
    , Int.to_string
        ( match op with
        | "Plus__" -> i1 + i2
        | "Minus__" -> i1 - i2
        | "Times__" -> i1 * i2
        | "Divide__" | "IntDivide__" -> i1 / i2
        | "Modulo__" -> i1 % i2
        | "Equals__" -> Bool.to_int (i1 = i2)
        | "NEquals__" -> Bool.to_int (i1 <> i2)
        | "Less__" -> Bool.to_int (i1 < i2)
        | "Leq__" -> Bool.to_int (i1 <= i2)
        | "Greater__" -> Bool.to_int (i1 > i2)
        | "Geq__" -> Bool.to_int (i1 >= i2)
        | s ->
            Common.FatalError.fatal_error_msg
              [%message "Not an int operator: " s] ) )

let apply_arithmetic_operator_real (op : string) r1 r2 =
  Expr.Fixed.Pattern.Lit
    ( Real
    , Float.to_string
        ( match op with
        | "Plus__" -> r1 +. r2
        | "Minus__" -> r1 -. r2
        | "Times__" -> r1 *. r2
        | "Divide__" -> r1 /. r2
        | s ->
            Common.FatalError.fatal_error_msg
              [%message "Not a real operator: " s] ) )

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
        | s ->
            Common.FatalError.fatal_error_msg
              [%message "Not a logical operator: " s] ) )

let is_multi_index = function
  | Index.MultiIndex _ | Upfrom _ | Between _ | All -> true
  | Single _ -> false

let rec eval_expr ?(preserve_stability = false) (e : Expr.Typed.t) =
  { e with
    pattern=
      ( match e.pattern with
      | Var _ | Lit (_, _) -> e.pattern
      | Promotion (expr, ut, ad) ->
          Promotion (eval_expr ~preserve_stability expr, ut, ad)
      | FunApp (kind, l) -> (
          let l = List.map ~f:(eval_expr ~preserve_stability) l in
          match kind with
          | UserDefined _ | CompilerInternal _ -> FunApp (kind, l)
          | StanLib (f, suffix, mem_type) ->
              let get_fun_or_op_rt_opt name l' =
                let argument_types =
                  List.map ~f:(fun x -> Expr.Typed.(adlevel_of x, type_of x)) l'
                in
                Operator.of_string_opt name
                |> Option.value_map
                     ~f:(fun op ->
                       Frontend.Typechecker.operator_stan_math_return_type op
                         argument_types
                       |> Option.map ~f:fst )
                     ~default:
                       (Frontend.Typechecker.stan_math_return_type name
                          argument_types ) in
              let try_partially_evaluate_stanlib e =
                Expr.Fixed.Pattern.(
                  match e with
                  | FunApp (StanLib (f', suffix', mem_type), l') -> (
                    match get_fun_or_op_rt_opt f' l' with
                    | Some _ -> FunApp (StanLib (f', suffix', mem_type), l')
                    | None -> FunApp (StanLib (f, suffix, mem_type), l) )
                  | e -> e) in
              let lub_mem_pat lst =
                Common.Helpers.lub_mem_pat (List.cons mem_type lst) in
              try_partially_evaluate_stanlib
                ( match (f, l) with
                (* TODO: deal with tilde statements and unnormalized distributions properly here *)
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("inv_logit", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Plus__", FnPlain, mem2)
                                      , [ alpha
                                        ; { pattern=
                                              FunApp
                                                ( StanLib
                                                    ("Times__", FnPlain, mem3)
                                                , [x; beta] )
                                          ; _ } ] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3] in
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("inv_logit", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Plus__", FnPlain, mem2)
                                      , [ { pattern=
                                              FunApp
                                                ( StanLib
                                                    ("Times__", FnPlain, mem3)
                                                , [x; beta] )
                                          ; _ }; alpha ] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3] in
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("inv_logit", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "bernoulli_logit_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ alpha
                              ; { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "bernoulli_logit_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ }; alpha ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "bernoulli_logit_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp (StanLib ("Times__", FnPlain, mem), [x; beta])
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib
                          ("bernoulli_logit_glm_lpmf", suffix, lub_mem_pat [mem])
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp (StanLib ("inv_logit", FnPlain, mem), [alpha])
                      ; _ } ] ) ->
                    FunApp
                      ( StanLib
                          ("bernoulli_logit_lpmf", suffix, lub_mem_pat [mem])
                      , [y; alpha] )
                | ( "bernoulli_rng"
                  , [ { pattern=
                          FunApp (StanLib ("inv_logit", FnPlain, mem), [alpha])
                      ; _ } ] ) ->
                    FunApp
                      ( StanLib
                          ("bernoulli_logit_rng", suffix, lub_mem_pat [mem])
                      , [alpha] )
                | ( "binomial_lpmf"
                  , [ y; n
                    ; { pattern=
                          FunApp (StanLib ("inv_logit", FnPlain, mem), [alpha])
                      ; _ } ] ) ->
                    FunApp
                      ( StanLib
                          ("binomial_logit_lpmf", suffix, lub_mem_pat [mem])
                      , [y; n; alpha] )
                | ( "categorical_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp (StanLib ("inv_logit", FnPlain, mem), [alpha])
                      ; _ } ] ) ->
                    FunApp
                      ( StanLib
                          ("categorical_logit_lpmf", suffix, lub_mem_pat [mem])
                      , [y; alpha] )
                | ( "categorical_rng"
                  , [ { pattern=
                          FunApp (StanLib ("inv_logit", FnPlain, mem), [alpha])
                      ; _ } ] ) ->
                    FunApp
                      ( StanLib
                          ("categorical_logit_rng", suffix, lub_mem_pat [mem])
                      , [alpha] )
                | "columns_dot_product", [x; y] when Expr.Typed.equal x y ->
                    FunApp (StanLib ("columns_dot_self", suffix, mem_type), [x])
                | "dot_product", [x; y] when Expr.Typed.equal x y ->
                    FunApp (StanLib ("dot_self", suffix, mem_type), [x])
                | ( "inv"
                  , [{pattern= FunApp (StanLib ("sqrt", FnPlain, mem), l); _}] )
                  ->
                    FunApp (StanLib ("inv_sqrt", suffix, mem), l)
                | ( "inv"
                  , [ { pattern= FunApp (StanLib ("square", FnPlain, mem), [x])
                      ; _ } ] ) ->
                    FunApp
                      (StanLib ("inv_square", suffix, lub_mem_pat [mem]), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("Minus__", FnPlain, mem1)
                            , [ y
                              ; { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem2), [x])
                                ; _ } ] )
                      ; _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("log1m_exp", suffix, lub_mem), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("Minus__", FnPlain, mem1)
                            , [ y
                              ; { pattern=
                                    FunApp
                                      (StanLib ("inv_logit", FnPlain, mem2), [x])
                                ; _ } ] )
                      ; _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("log1m_inv_logit", suffix, lub_mem), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp (StanLib ("Minus__", FnPlain, mem), [y; x])
                      ; _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib ("log1m", suffix, lub_mem_pat [mem]), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ y
                              ; { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem2), [x])
                                ; _ } ] )
                      ; _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("log1p_exp", suffix, lub_mem), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp (StanLib ("Plus__", FnPlain, mem), [y; x])
                      ; _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib ("log1p", suffix, lub_mem_pat [mem]), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("fabs", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("determinant", FnPlain, mem2)
                                      , [x] )
                                ; _ } ] )
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("log_determinant", suffix, lub_mem), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("Minus__", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem2), [x])
                                ; _ }
                              ; { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem3), [y])
                                ; _ } ] )
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3] in
                    FunApp (StanLib ("log_diff_exp", suffix, lub_mem), [x; y])
                (* TODO: log_mix?*)
                | ( "log"
                  , [ { pattern=
                          FunApp (StanLib ("falling_factorial", FnPlain, mem), l)
                      ; _ } ] ) ->
                    FunApp
                      ( StanLib
                          ("log_falling_factorial", suffix, lub_mem_pat [mem])
                      , l )
                | ( "log"
                  , [ { pattern=
                          FunApp (StanLib ("rising_factorial", FnPlain, mem), l)
                      ; _ } ] ) ->
                    FunApp
                      ( StanLib
                          ("log_rising_factorial", suffix, lub_mem_pat [mem])
                      , l )
                | ( "log"
                  , [ { pattern= FunApp (StanLib ("inv_logit", FnPlain, mem), l)
                      ; _ } ] ) ->
                    FunApp
                      (StanLib ("log_inv_logit", suffix, lub_mem_pat [mem]), l)
                | ( "log"
                  , [{pattern= FunApp (StanLib ("softmax", FnPlain, mem), l); _}]
                  ) ->
                    FunApp
                      (StanLib ("log_softmax", suffix, lub_mem_pat [mem]), l)
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("sum", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem2), l)
                                ; _ } ] )
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("log_sum_exp", suffix, lub_mem), l)
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem2), [x])
                                ; _ }
                              ; { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem3), [y])
                                ; _ } ] )
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3] in
                    FunApp (StanLib ("log_sum_exp", suffix, lub_mem), [x; y])
                | ( "multi_normal_lpdf"
                  , [ y; mu
                    ; { pattern=
                          FunApp (StanLib ("inverse", FnPlain, mem), [tau])
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("multi_normal_prec_lpdf", suffix, lub_mem)
                      , [y; mu; tau] )
                | ( "neg_binomial_2_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Plus__", FnPlain, mem2)
                                      , [ alpha
                                        ; { pattern=
                                              FunApp
                                                ( StanLib
                                                    ("Times__", FnPlain, mem3)
                                                , [x; beta] )
                                          ; _ } ] )
                                ; _ } ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Plus__", FnPlain, mem2)
                                      , [ { pattern=
                                              FunApp
                                                ( StanLib
                                                    ("Times__", FnPlain, mem3)
                                                , [x; beta] )
                                          ; _ }; alpha ] )
                                ; _ } ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ } ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ alpha
                              ; { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ } ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ }; alpha ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp (StanLib ("Times__", FnPlain, mem), [x; beta])
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta; sigma] )
                | ( "neg_binomial_2_lpmf"
                  , [ y
                    ; {pattern= FunApp (StanLib ("exp", FnPlain, mem), [eta]); _}
                    ; phi ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_lpmf", suffix, lub_mem)
                      , [y; eta; phi] )
                | ( "neg_binomial_2_rng"
                  , [ {pattern= FunApp (StanLib ("exp", FnPlain, mem), [eta]); _}
                    ; phi ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_rng", suffix, lub_mem)
                      , [eta; phi] )
                | ( "normal_lpdf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ alpha
                              ; { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ } ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("normal_id_glm_lpdf", suffix, lub_mem)
                      , [y; x; alpha; beta; sigma] )
                | ( "normal_lpdf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ }; alpha ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("normal_id_glm_lpdf", suffix, lub_mem)
                      , [y; x; alpha; beta; sigma] )
                | ( "normal_lpdf"
                  , [ y
                    ; { pattern=
                          FunApp (StanLib ("Times__", FnPlain, mem), [x; beta])
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("normal_id_glm_lpdf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta; sigma] )
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Plus__", FnPlain, mem2)
                                      , [ alpha
                                        ; { pattern=
                                              FunApp
                                                ( StanLib
                                                    ("Times__", FnPlain, mem3)
                                                , [x; beta] )
                                          ; _ } ] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Plus__", FnPlain, mem2)
                                      , [ { pattern=
                                              FunApp
                                                ( StanLib
                                                    ("Times__", FnPlain, mem3)
                                                , [x; beta] )
                                          ; _ }; alpha ] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "poisson_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ alpha
                              ; { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "poisson_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("Plus__", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [x; beta] )
                                ; _ }; alpha ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "poisson_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp (StanLib ("Times__", FnPlain, mem), [x; beta])
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "poisson_lpmf"
                  , [ y
                    ; {pattern= FunApp (StanLib ("exp", FnPlain, mem), [eta]); _}
                    ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      (StanLib ("poisson_log_lpmf", suffix, lub_mem), [y; eta])
                | ( "poisson_rng"
                  , [{pattern= FunApp (StanLib ("exp", FnPlain, mem), [eta]); _}]
                  ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("poisson_log_rng", suffix, lub_mem), [eta])
                | "pow", [y; x] when is_int 2 y ->
                    FunApp (StanLib ("exp2", suffix, mem_type), [x])
                | "rows_dot_product", [x; y] when Expr.Typed.equal x y ->
                    FunApp (StanLib ("rows_dot_self", suffix, mem_type), [x])
                | "pow", [x; {pattern= Lit (Int, "2"); _}] ->
                    FunApp (StanLib ("square", suffix, mem_type), [x])
                | "pow", [x; {pattern= Lit (Real, "0.5"); _}] ->
                    FunApp (StanLib ("sqrt", suffix, mem_type), [x])
                | ( "pow"
                  , [ x
                    ; { pattern=
                          FunApp (StanLib ("Divide__", FnPlain, mem), [y; z])
                      ; _ } ] )
                  when is_int 1 y && is_int 2 z ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("sqrt", suffix, lub_mem), [x])
                    (* This is wrong; if both are type UInt the exponent is rounds down to zero. *)
                | ( "square"
                  , [{pattern= FunApp (StanLib ("sd", FnPlain, mem), [x]); _}] )
                  ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("variance", suffix, lub_mem), [x])
                | "sqrt", [x] when is_int 2 x ->
                    FunApp (StanLib ("sqrt2", suffix, mem_type), [])
                | ( "sum"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("square", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Minus__", FnPlain, mem2)
                                      , [x; y] )
                                ; _ } ] )
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      (StanLib ("squared_distance", suffix, lub_mem), [x; y])
                | ( "sum"
                  , [ { pattern= FunApp (StanLib ("diagonal", FnPlain, mem), l)
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("trace", suffix, lub_mem), l)
                | ( "trace"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("Times__", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [ { pattern=
                                              FunApp
                                                ( StanLib
                                                    ("Times__", FnPlain, mem3)
                                                , [ d
                                                  ; { pattern=
                                                        FunApp
                                                          ( StanLib
                                                              ( "transpose"
                                                              , FnPlain
                                                              , mem4 )
                                                          , [b] )
                                                    ; _ } ] )
                                          ; _ }; a ] )
                                ; _ }; c ] )
                      ; _ } ] )
                  when Expr.Typed.equal b c ->
                    let lub_mem = lub_mem_pat [mem1; mem2; mem3; mem4] in
                    FunApp
                      ( StanLib ("trace_gen_quad_form", suffix, lub_mem)
                      , [d; a; b] )
                | ( "trace"
                  , [ { pattern=
                          FunApp (StanLib ("quad_form", FnPlain, mem), [a; b])
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("trace_quad_form", suffix, lub_mem), [a; b])
                | ( "Minus__"
                  , [x; {pattern= FunApp (StanLib ("erf", FnPlain, mem), l); _}]
                  )
                  when is_int 1 x ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("erfc", suffix, lub_mem), l)
                | ( "Minus__"
                  , [x; {pattern= FunApp (StanLib ("erfc", FnPlain, mem), l); _}]
                  )
                  when is_int 1 x ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("erf", suffix, lub_mem), l)
                | ( "Minus__"
                  , [{pattern= FunApp (StanLib ("exp", FnPlain, mem), l'); _}; x]
                  )
                  when is_int 1 x && not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("expm1", suffix, lub_mem), l')
                | ( "Plus__"
                  , [ { pattern=
                          FunApp (StanLib ("Times__", FnPlain, mem), [x; y])
                      ; _ }; z ] )
                  when (not preserve_stability)
                       && not
                            ( UnsizedType.is_eigen_type x.meta.type_
                            && UnsizedType.is_eigen_type y.meta.type_ ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("fma", suffix, lub_mem), [x; y; z])
                | ( "Plus__"
                  , [ z
                    ; { pattern=
                          FunApp (StanLib ("Times__", FnPlain, mem), [x; y])
                      ; _ } ] )
                  when (not preserve_stability)
                       && not
                            ( UnsizedType.is_eigen_type x.meta.type_
                            && UnsizedType.is_eigen_type y.meta.type_ ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("fma", suffix, lub_mem), [x; y; z])
                | ( "Plus__"
                  , [ { pattern=
                          FunApp
                            ( StanLib
                                (("elt_multiply" | "EltTimes__"), FnPlain, mem)
                            , [x; y] )
                      ; _ }; z ] )
                  when not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("fma", suffix, lub_mem), [x; y; z])
                | ( "Plus__"
                  , [ z
                    ; { pattern=
                          FunApp
                            ( StanLib
                                (("elt_multiply" | "EltTimes__"), FnPlain, mem)
                            , [x; y] )
                      ; _ } ] )
                  when not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("fma", suffix, lub_mem), [x; y; z])
                | ( "Minus__"
                  , [ x
                    ; {pattern= FunApp (StanLib ("gamma_p", FnPlain, mem), l); _}
                    ] )
                  when is_int 1 x ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("gamma_q", suffix, lub_mem), l)
                | ( "Minus__"
                  , [ x
                    ; {pattern= FunApp (StanLib ("gamma_q", FnPlain, mem), l); _}
                    ] )
                  when is_int 1 x ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("gamma_p", suffix, lub_mem), l)
                | ( "Times__"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("matrix_exp", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [t; a] )
                                ; _ } ] )
                      ; _ }; b ] )
                  when Expr.Typed.type_of t = UInt
                       || Expr.Typed.type_of t = UReal ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("scale_matrix_exp_multiply", suffix, lub_mem)
                      , [t; a; b] )
                | ( "Times__"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("matrix_exp", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      ( StanLib ("Times__", FnPlain, mem2)
                                      , [a; t] )
                                ; _ } ] )
                      ; _ }; b ] )
                  when Expr.Typed.type_of t = UInt
                       || Expr.Typed.type_of t = UReal ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp
                      ( StanLib ("scale_matrix_exp_multiply", suffix, lub_mem)
                      , [t; a; b] )
                | ( "Times__"
                  , [ { pattern=
                          FunApp (StanLib ("matrix_exp", FnPlain, mem), [a])
                      ; _ }; b ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      (StanLib ("matrix_exp_multiply", suffix, lub_mem), [a; b])
                | ( "Times__"
                  , [ x
                    ; {pattern= FunApp (StanLib ("log", FnPlain, mem), [y]); _}
                    ] )
                 |( "Times__"
                  , [ {pattern= FunApp (StanLib ("log", FnPlain, mem), [y]); _}
                    ; x ] )
                  when not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("lmultiply", suffix, lub_mem), [x; y])
                | ( "Times__"
                  , [ { pattern=
                          FunApp (StanLib ("diag_matrix", FnPlain, mem1), [v])
                      ; _ }
                    ; { pattern=
                          FunApp
                            ( StanLib ("diag_post_multiply", FnPlain, mem2)
                            , [a; w] )
                      ; _ } ] )
                  when Expr.Typed.equal v w ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("quad_form_diag", suffix, lub_mem), [a; v])
                | ( "Times__"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("diag_pre_multiply", FnPlain, mem1)
                            , [v; a] )
                      ; _ }
                    ; { pattern=
                          FunApp (StanLib ("diag_matrix", FnPlain, mem2), [w])
                      ; _ } ] )
                  when Expr.Typed.equal v w ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("quad_form_diag", suffix, lub_mem), [a; v])
                | ( "Times__"
                  , [ { pattern=
                          FunApp (StanLib ("transpose", FnPlain, mem1), [b])
                      ; _ }
                    ; { pattern=
                          FunApp (StanLib ("Times__", FnPlain, mem2), [a; c])
                      ; _ } ] )
                  when Expr.Typed.equal b c ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("quad_form", suffix, lub_mem), [a; b])
                | ( "Times__"
                  , [ { pattern=
                          FunApp
                            ( StanLib ("Times__", FnPlain, mem1)
                            , [ { pattern=
                                    FunApp
                                      (StanLib ("transpose", FnPlain, mem2), [b])
                                ; _ }; a ] )
                      ; _ }; c ] )
                  when Expr.Typed.equal b c ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("quad_form", suffix, lub_mem), [a; b])
                | ( "Times__"
                  , [ e1'
                    ; { pattern=
                          FunApp (StanLib ("diag_matrix", FnPlain, mem), [v])
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      (StanLib ("diag_post_multiply", suffix, lub_mem), [e1'; v])
                | ( "Times__"
                  , [ { pattern=
                          FunApp (StanLib ("diag_matrix", FnPlain, mem), [v])
                      ; _ }; e2' ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      (StanLib ("diag_pre_multiply", suffix, lub_mem), [v; e2'])
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
                | ( ("Divide__" | "IntDivide__")
                  , [{meta= {type_= UInt; _}; _}; {pattern= Lit (Int, i2); _}] )
                  when Int.of_string i2 = 0 ->
                    raise (Rejected (e.meta.loc, "Integer division by zero"))
                | op, [{pattern= Lit (Int, i1); _}; {pattern= Lit (Int, i2); _}]
                  -> (
                  match op with
                  | "Plus__" | "Minus__" | "Times__" | "Divide__"
                   |"IntDivide__" | "Modulo__" | "Or__" | "And__" | "Equals__"
                   |"NEquals__" | "Less__" | "Leq__" | "Greater__" | "Geq__" ->
                      apply_operator_int op (Int.of_string i1)
                        (Int.of_string i2)
                  | _ -> FunApp (kind, l) )
                | ( op
                  , [{pattern= Lit (Real, i1); _}; {pattern= Lit (Real, i2); _}]
                  )
                 |op, [{pattern= Lit (Int, i1); _}; {pattern= Lit (Real, i2); _}]
                 |op, [{pattern= Lit (Real, i1); _}; {pattern= Lit (Int, i2); _}]
                  -> (
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
        match
          ( eval_expr ~preserve_stability e1
          , eval_expr ~preserve_stability e2
          , eval_expr ~preserve_stability e3 )
        with
        | x, _, e3' when is_int 0 x -> e3'.pattern
        | {pattern= Lit (Int, _); _}, e2', _ -> e2'.pattern
        | e1', e2', e3' -> TernaryIf (e1', e2', e3') )
      | EAnd (e1, e2) -> (
        match
          (eval_expr ~preserve_stability e1, eval_expr ~preserve_stability e2)
        with
        | {pattern= Lit (Int, s1); _}, {pattern= Lit (Int, s2); _} ->
            let i1, i2 = (Int.of_string s1, Int.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (i1 <> 0 && i2 <> 0)))
        | {pattern= Lit (_, s1); _}, {pattern= Lit (_, s2); _} ->
            let r1, r2 = (Float.of_string s1, Float.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (r1 <> 0. && r2 <> 0.)))
        | e1', e2' -> EAnd (e1', e2') )
      | EOr (e1, e2) -> (
        match
          (eval_expr ~preserve_stability e1, eval_expr ~preserve_stability e2)
        with
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
        , ( Single ({meta= Expr.Typed.Meta.{type_= UInt; _}; _} as single_e) as
          single )
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
               , outer_tl ) )
      | inner_singles, All :: inner_tl ->
          (* v[:x][i] -> v[i] *)
          (* v[:][i] -> v[i] *)
          (* XXX generate check *)
          simplify_index_expr
            (Indexed
               ( { pattern= Indexed (obj, inner_singles @ [single] @ inner_tl)
                 ; meta }
               , outer_tl ) )
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
               , outer_tl ) )
      | inner_singles, (([] | Single _ :: _) as multis) ->
          Common.FatalError.fatal_error_msg
            [%message
              " There must be a multi-index."
                (inner_singles : Expr.Typed.t Index.t list)
                (multis : Expr.Typed.t Index.t list)] )
    | e -> e)

let remove_trailing_alls_expr = function
  | Expr.Fixed.Pattern.Indexed (obj, indices) ->
      (* a[2][:] -> a[2] *)
      let rec remove_trailing_alls indices =
        match List.rev indices with
        | Index.All :: tl -> remove_trailing_alls (List.rev tl)
        | _ -> indices in
      Expr.Fixed.Pattern.Indexed (obj, remove_trailing_alls indices)
  | e -> e

let rec simplify_indices_expr expr =
  Expr.Fixed.(
    let pattern =
      expr.pattern |> remove_trailing_alls_expr |> simplify_index_expr
      |> Expr.Fixed.Pattern.map simplify_indices_expr in
    {expr with pattern})

let try_eval_expr expr = try eval_expr expr with Rejected _ -> expr

let rec eval_stmt s =
  try
    Stmt.Fixed.
      { s with
        pattern=
          Pattern.map
            (Fn.compose eval_expr simplify_indices_expr)
            eval_stmt s.pattern }
  with Rejected (loc, m) ->
    { Stmt.Fixed.pattern=
        NRFunApp (CompilerInternal FnReject, [Expr.Helpers.str m])
    ; meta= loc }

let eval_prog = Program.map try_eval_expr eval_stmt
