(* A partial evaluator for use in static analysis and optimization *)

open Std
open Middle

exception Rejected of Location_span.t * string

let rec is_int query Expr.{pattern; _} =
  match pattern with
  | Lit (Int, i) | Lit (Real, i) -> Float.of_string i = Float.of_int query
  | Promotion (e, _, _) -> is_int query e
  | _ -> false

let apply_prefix_operator_int (op : Operator.t) i =
  Expr.Pattern.Lit
    ( Int
    , Int.to_string
        (match op with
        | PPlus -> i
        | PMinus -> -i
        | PNot -> if i = 0 then 1 else 0
        | _ ->
            Common.ICE.(
              internal_errorf "Not an int prefix operator: %t" [Operator.pp $ op])
            [@coverage off]) )

let apply_prefix_operator_real (op : Operator.t) i =
  Expr.Pattern.Lit
    ( Real
    , Float.to_string
        (match op with
        | PPlus -> i
        | PMinus -> -.i
        | _ ->
            Common.ICE.(
              internal_errorf "Not a real prefix operator: %t" [Operator.pp $ op])
            [@coverage off]) )

let apply_operator_int (op : Operator.t) i1 i2 =
  Expr.Pattern.Lit
    ( Int
    , Int.to_string
        (match op with
        | Plus -> i1 + i2
        | Minus -> i1 - i2
        | Times -> i1 * i2
        | Divide | IntDivide -> i1 / i2
        | Modulo -> Int.rem i1 i2
        | Equals -> Bool.to_int (i1 = i2)
        | NEquals -> Bool.to_int (i1 <> i2)
        | Less -> Bool.to_int (i1 < i2)
        | Leq -> Bool.to_int (i1 <= i2)
        | Greater -> Bool.to_int (i1 > i2)
        | Geq -> Bool.to_int (i1 >= i2)
        | _ ->
            Common.ICE.(
              internal_errorf "Not an int operator: %t" [Operator.pp $ op])
            [@coverage off]) )

let apply_arithmetic_operator_real (op : Operator.t) r1 r2 =
  Expr.Pattern.Lit
    ( Real
    , Float.to_string
        (match op with
        | Plus -> r1 +. r2
        | Minus -> r1 -. r2
        | Times -> r1 *. r2
        | Divide -> r1 /. r2
        | _ ->
            Common.ICE.(
              internal_errorf "Not a real operator: %t" [Operator.pp $ op])
            [@coverage off]) )

let apply_logical_operator_real (op : Operator.t) r1 r2 =
  Expr.Pattern.Lit
    ( Int
    , Int.to_string
        (match op with
        | Equals -> Bool.to_int (r1 = r2)
        | NEquals -> Bool.to_int (r1 <> r2)
        | Less -> Bool.to_int (r1 < r2)
        | Leq -> Bool.to_int (r1 <= r2)
        | Greater -> Bool.to_int (r1 > r2)
        | Geq -> Bool.to_int (r1 >= r2)
        | _ ->
            Common.ICE.(
              internal_errorf "Not a logical operator: %t" [Operator.pp $ op])
            [@coverage off]) )

let stan_operator_return_type op args =
  let arg_types = List.map ~f:Expr.Typed.fun_arg args in
  Frontend.Typechecker.operator_stan_math_return_type op arg_types
  |> Option.map ~f:fst

let stan_math_return_type name args =
  let arg_types = List.map ~f:Expr.Typed.fun_arg args in
  Frontend.Typechecker.stan_math_return_type name arg_types

let is_multi_index = function
  | Index.MultiIndex _ | Upfrom _ | Between _ | All -> true
  | Single _ -> false

let rec eval_expr ?(preserve_stability = false) (e : Expr.Typed.t) =
  { e with
    pattern=
      (match e.pattern with
      | Var _ | Lit (_, _) -> e.pattern
      | Promotion (expr, ut, ad) ->
          Promotion (eval_expr ~preserve_stability expr, ut, ad)
      | FunApp (kind, l) -> (
          let l = List.map ~f:(eval_expr ~preserve_stability) l in
          match kind with
          | UserDefined _ | CompilerInternal _ -> FunApp (kind, l)
          | Operator op ->
              let try_partially_evaluate_stanlib e' =
                Expr.Pattern.(
                  match e' with
                  | FunApp (Operator op', l')
                    when not (Operator.compare op op' = 0 && l == l') -> (
                      match stan_operator_return_type op' l' with
                      | Some _ -> e'
                      | None -> e.pattern)
                  | FunApp (StanLib (f', _, _), l') -> (
                      match stan_math_return_type f' l' with
                      | Some _ -> e'
                      | None -> e.pattern)
                  | _ -> e') in
              try_partially_evaluate_stanlib
                (match (op, l) with
                | ( Plus
                  , [ ({pattern= Lit (Imaginary, i); _} as im)
                    ; ({pattern= Lit ((Real | Int), _); _} as r) ] )
                 |( Plus
                  , [ ({pattern= Lit ((Real | Int), _); _} as r)
                    ; ({pattern= Lit (Imaginary, i); _} as im) ] )
                 |( Plus
                  , [ ({pattern= Lit (Imaginary, i); _} as im)
                    ; { pattern=
                          Promotion
                            ( ({pattern= Lit ((Real | Int), _); _} as r)
                            , UComplex
                            , _ )
                      ; _ } ] )
                 |( Plus
                  , [ { pattern=
                          Promotion
                            ( ({pattern= Lit ((Real | Int), _); _} as r)
                            , UComplex
                            , _ )
                      ; _ }; ({pattern= Lit (Imaginary, i); _} as im) ] ) ->
                    let im_part =
                      Expr.
                        { pattern= Lit (Real, i)
                        ; meta= {im.meta with type_= UReal} } in
                    FunApp (StanLib ("to_complex", FnPlain, AoS), [r; im_part])
                | ( Minus
                  , [x; {pattern= FunApp (StanLib ("erf", FnPlain, mem), l); _}]
                  )
                  when is_int 1 x ->
                    FunApp (StanLib ("erfc", FnPlain, mem), l)
                | ( Minus
                  , [x; {pattern= FunApp (StanLib ("erfc", FnPlain, mem), l); _}]
                  )
                  when is_int 1 x ->
                    FunApp (StanLib ("erf", FnPlain, mem), l)
                | ( Minus
                  , [{pattern= FunApp (StanLib ("exp", FnPlain, mem), l'); _}; x]
                  )
                  when is_int 1 x && not preserve_stability ->
                    FunApp (StanLib ("expm1", FnPlain, mem), l')
                | Plus, [{pattern= FunApp (Operator Times, [x; y]); _}; z]
                  when (not preserve_stability)
                       && not
                            (UnsizedType.is_eigen_type x.meta.type_
                            && UnsizedType.is_eigen_type y.meta.type_) ->
                    FunApp (StanLib ("fma", FnPlain, AoS), [x; y; z])
                | Plus, [z; {pattern= FunApp (Operator Times, [x; y]); _}]
                  when (not preserve_stability)
                       && not
                            (UnsizedType.is_eigen_type x.meta.type_
                            && UnsizedType.is_eigen_type y.meta.type_) ->
                    FunApp (StanLib ("fma", FnPlain, AoS), [x; y; z])
                | Plus, [{pattern= FunApp (Operator EltTimes, [x; y]); _}; z]
                 |Plus, [z; {pattern= FunApp (Operator EltTimes, [x; y]); _}]
                  when not preserve_stability ->
                    FunApp (StanLib ("fma", FnPlain, AoS), [x; y; z])
                | ( Minus
                  , [ x
                    ; {pattern= FunApp (StanLib ("gamma_p", FnPlain, mem), l); _}
                    ] )
                  when is_int 1 x ->
                    FunApp (StanLib ("gamma_q", FnPlain, mem), l)
                | ( Minus
                  , [ x
                    ; {pattern= FunApp (StanLib ("gamma_q", FnPlain, mem), l); _}
                    ] )
                  when is_int 1 x ->
                    FunApp (StanLib ("gamma_p", FnPlain, mem), l)
                | ( Times
                  , [ { pattern=
                          FunApp
                            ( StanLib ("matrix_exp", FnPlain, mem)
                            , [{pattern= FunApp (Operator Times, [t; a]); _}] )
                      ; _ }; b ] )
                  when Expr.Typed.type_of t = UInt
                       || Expr.Typed.type_of t = UReal ->
                    FunApp
                      ( StanLib ("scale_matrix_exp_multiply", FnPlain, mem)
                      , [t; a; b] )
                | ( Times
                  , [ { pattern=
                          FunApp
                            ( StanLib ("matrix_exp", FnPlain, mem)
                            , [{pattern= FunApp (Operator Times, [a; t]); _}] )
                      ; _ }; b ] )
                  when Expr.Typed.type_of t = UInt
                       || Expr.Typed.type_of t = UReal ->
                    FunApp
                      ( StanLib ("scale_matrix_exp_multiply", FnPlain, mem)
                      , [t; a; b] )
                | ( Times
                  , [ { pattern=
                          FunApp (StanLib ("matrix_exp", FnPlain, mem), [a])
                      ; _ }; b ] ) ->
                    FunApp
                      (StanLib ("matrix_exp_multiply", FnPlain, mem), [a; b])
                | ( Times
                  , [ x
                    ; {pattern= FunApp (StanLib ("log", FnPlain, mem), [y]); _}
                    ] )
                 |( Times
                  , [ {pattern= FunApp (StanLib ("log", FnPlain, mem), [y]); _}
                    ; x ] )
                  when not preserve_stability ->
                    FunApp (StanLib ("lmultiply", FnPlain, mem), [x; y])
                | ( Times
                  , [ { pattern=
                          FunApp (StanLib ("diag_matrix", FnPlain, mem1), [v])
                      ; _ }
                    ; { pattern=
                          FunApp
                            ( StanLib ("diag_post_multiply", FnPlain, mem2)
                            , [a; w] )
                      ; _ } ] )
                  when Expr.Typed.equal v w ->
                    let lub_mem = Mem_pattern.lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("quad_form_diag", FnPlain, lub_mem), [a; v])
                | ( Times
                  , [ { pattern=
                          FunApp
                            ( StanLib ("diag_pre_multiply", FnPlain, mem1)
                            , [v; a] )
                      ; _ }
                    ; { pattern=
                          FunApp (StanLib ("diag_matrix", FnPlain, mem2), [w])
                      ; _ } ] )
                  when Expr.Typed.equal v w ->
                    let lub_mem = Mem_pattern.lub_mem_pat [mem1; mem2] in
                    FunApp (StanLib ("quad_form_diag", FnPlain, lub_mem), [a; v])
                | ( Times
                  , [ { pattern=
                          FunApp
                            ( ( Operator Transpose
                              | StanLib ("transpose", FnPlain, _) )
                            , [b] )
                      ; _ }; {pattern= FunApp (Operator Times, [a; c]); _} ] )
                  when Expr.Typed.equal b c ->
                    FunApp (StanLib ("quad_form", FnPlain, AoS), [a; b])
                | ( Times
                  , [ { pattern=
                          FunApp
                            ( Operator Times
                            , [ { pattern=
                                    FunApp
                                      ( ( Operator Transpose
                                        | StanLib ("transpose", _, _) )
                                      , [b] )
                                ; _ }; a ] )
                      ; _ }; c ] )
                  when Expr.Typed.equal b c ->
                    FunApp (StanLib ("quad_form", FnPlain, AoS), [a; b])
                | ( Times
                  , [ e1'
                    ; { pattern=
                          FunApp (StanLib ("diag_matrix", FnPlain, mem), [v])
                      ; _ } ] ) ->
                    FunApp
                      (StanLib ("diag_post_multiply", FnPlain, mem), [e1'; v])
                | ( Times
                  , [ { pattern=
                          FunApp (StanLib ("diag_matrix", FnPlain, mem), [v])
                      ; _ }; e2' ] ) ->
                    FunApp
                      (StanLib ("diag_pre_multiply", FnPlain, mem), [v; e2'])
                    (* Constant folding for operators *)
                | (PPlus | PMinus | PNot), [{pattern= Lit (Int, i); _}] ->
                    apply_prefix_operator_int op (Int.of_string i)
                | (PPlus | PMinus), [{pattern= Lit (Real, r); _}] ->
                    apply_prefix_operator_real op (Float.of_string r)
                | ( (Divide | IntDivide | Modulo)
                  , [{meta= {type_= UInt; _}; _}; {pattern= Lit (Int, i2); _}] )
                  when Int.of_string i2 = 0 ->
                    raise (Rejected (e.meta.loc, "Integer division by zero"))
                | ( ( Plus | Minus | Times | Divide | IntDivide | Modulo
                    | Equals | NEquals | Less | Leq | Greater | Geq )
                  , [{pattern= Lit (Int, i1); _}; {pattern= Lit (Int, i2); _}] )
                  ->
                    apply_operator_int op (Int.of_string i1) (Int.of_string i2)
                | ( (Plus | Minus | Times | Divide)
                  , ( [ {pattern= Lit (Real, i1); _}
                      ; {pattern= Lit (Real, i2); _} ]
                    | [{pattern= Lit (Int, i1); _}; {pattern= Lit (Real, i2); _}]
                    | [{pattern= Lit (Real, i1); _}; {pattern= Lit (Int, i2); _}]
                      ) ) ->
                    apply_arithmetic_operator_real op (Float.of_string i1)
                      (Float.of_string i2)
                | ( (Equals | NEquals | Less | Leq | Greater | Geq)
                  , ( [ {pattern= Lit (Real, i1); _}
                      ; {pattern= Lit (Real, i2); _} ]
                    | [{pattern= Lit (Int, i1); _}; {pattern= Lit (Real, i2); _}]
                    | [{pattern= Lit (Real, i1); _}; {pattern= Lit (Int, i2); _}]
                      ) ) ->
                    apply_logical_operator_real op (Float.of_string i1)
                      (Float.of_string i2)
                | _ -> FunApp (kind, l))
          | StanLib (f, suffix, mem_type) ->
              let try_partially_evaluate_stanlib e' =
                Expr.Pattern.(
                  match e' with
                  | FunApp (StanLib (f', _, _), l')
                    when not (String.equal f f' && l == l') -> (
                      match stan_math_return_type f' l' with
                      | Some _ -> e'
                      | None -> e.pattern)
                  | FunApp (Operator op', l') -> (
                      match stan_operator_return_type op' l' with
                      | Some _ -> e'
                      | None -> e.pattern)
                  | _ -> e') in
              let lub_mem_pat lst =
                Mem_pattern.lub_mem_pat (List.cons mem_type lst) in
              try_partially_evaluate_stanlib
                (match (f, l) with
                (* TODO: deal with tilde statements and unnormalized
                   distributions properly here *)
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("inv_logit", FnPlain, mem)
                            , [ { pattern=
                                    FunApp
                                      ( Operator Plus
                                      , [ alpha
                                        ; { pattern=
                                              FunApp (Operator Times, [x; beta])
                                          ; _ } ] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("inv_logit", FnPlain, mem)
                            , [ { pattern=
                                    FunApp
                                      ( Operator Plus
                                      , [ { pattern=
                                              FunApp (Operator Times, [x; beta])
                                          ; _ }; alpha ] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "bernoulli_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("inv_logit", FnPlain, mem)
                            , [{pattern= FunApp (Operator Times, [x; beta]); _}]
                            )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "bernoulli_logit_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( Operator Plus
                            , [ alpha
                              ; {pattern= FunApp (Operator Times, [x; beta]); _}
                              ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, mem_type)
                      , [y; x; alpha; beta] )
                | ( "bernoulli_logit_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( Operator Plus
                            , [ {pattern= FunApp (Operator Times, [x; beta]); _}
                              ; alpha ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, mem_type)
                      , [y; x; alpha; beta] )
                | ( "bernoulli_logit_lpmf"
                  , [y; {pattern= FunApp (Operator Times, [x; beta]); _}] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("bernoulli_logit_glm_lpmf", suffix, mem_type)
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
                          FunApp (StanLib ("softmax", FnPlain, mem), [alpha])
                      ; _ } ] ) ->
                    FunApp
                      ( StanLib
                          ("categorical_logit_lpmf", suffix, lub_mem_pat [mem])
                      , [y; alpha] )
                | ( "categorical_rng"
                  , [ { pattern=
                          FunApp (StanLib ("softmax", FnPlain, mem), [alpha])
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
                            ( Operator Minus
                            , [ y
                              ; { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem), [x])
                                ; _ } ] )
                      ; _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("log1m_exp", suffix, lub_mem), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( Operator Minus
                            , [ y
                              ; { pattern=
                                    FunApp
                                      (StanLib ("inv_logit", FnPlain, mem), [x])
                                ; _ } ] )
                      ; _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("log1m_inv_logit", suffix, lub_mem), [x])
                | "log", [{pattern= FunApp (Operator Minus, [y; x]); _}]
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib ("log1m", suffix, mem_type), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( Operator Plus
                            , [ y
                              ; { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem), [x])
                                ; _ } ] )
                      ; _ } ] )
                  when is_int 1 y && not preserve_stability ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("log1p_exp", suffix, lub_mem), [x])
                | "log", [{pattern= FunApp (Operator Plus, [y; x]); _}]
                  when is_int 1 y && not preserve_stability ->
                    FunApp (StanLib ("log1p", suffix, mem_type), [x])
                | ( "log"
                  , [ { pattern=
                          FunApp
                            ( StanLib (("fabs" | "abs"), FnPlain, mem1)
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
                            ( Operator Minus
                            , [ { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem1), [x])
                                ; _ }
                              ; { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem2), [y])
                                ; _ } ] )
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
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
                            ( Operator Plus
                            , [ { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem1), [x])
                                ; _ }
                              ; { pattern=
                                    FunApp (StanLib ("exp", FnPlain, mem2), [y])
                                ; _ } ] )
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem1; mem2] in
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
                            ( StanLib ("exp", FnPlain, mem)
                            , [ { pattern=
                                    FunApp
                                      ( Operator Plus
                                      , [ alpha
                                        ; { pattern=
                                              FunApp (Operator Times, [x; beta])
                                          ; _ } ] )
                                ; _ } ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
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
                                      ( Operator Plus
                                      , [ { pattern=
                                              FunApp (Operator Times, [x; beta])
                                          ; _ }; alpha ] )
                                ; _ } ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem1] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem)
                            , [{pattern= FunApp (Operator Times, [x; beta]); _}]
                            )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( Operator Plus
                            , [ alpha
                              ; {pattern= FunApp (Operator Times, [x; beta]); _}
                              ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, mem_type)
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( Operator Plus
                            , [ {pattern= FunApp (Operator Times, [x; beta]); _}
                              ; alpha ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, mem_type)
                      , [y; x; alpha; beta; sigma] )
                | ( "neg_binomial_2_log_lpmf"
                  , [y; {pattern= FunApp (Operator Times, [x; beta]); _}; sigma]
                  )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("neg_binomial_2_log_glm_lpmf", suffix, mem_type)
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
                            ( Operator Plus
                            , [ alpha
                              ; {pattern= FunApp (Operator Times, [x; beta]); _}
                              ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("normal_id_glm_lpdf", suffix, mem_type)
                      , [y; x; alpha; beta; sigma] )
                | ( "normal_lpdf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( Operator Plus
                            , [ {pattern= FunApp (Operator Times, [x; beta]); _}
                              ; alpha ] )
                      ; _ }; sigma ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("normal_id_glm_lpdf", suffix, mem_type)
                      , [y; x; alpha; beta; sigma] )
                | ( "normal_lpdf"
                  , [y; {pattern= FunApp (Operator Times, [x; beta]); _}; sigma]
                  )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("normal_id_glm_lpdf", suffix, mem_type)
                      , [y; x; Expr.Helpers.zero; beta; sigma] )
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem)
                            , [ { pattern=
                                    FunApp
                                      ( Operator Plus
                                      , [ alpha
                                        ; { pattern=
                                              FunApp (Operator Times, [x; beta])
                                          ; _ } ] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem)
                            , [ { pattern=
                                    FunApp
                                      ( Operator Plus
                                      , [ { pattern=
                                              FunApp (Operator Times, [x; beta])
                                          ; _ }; alpha ] )
                                ; _ } ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; alpha; beta] )
                | ( "poisson_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( StanLib ("exp", FnPlain, mem)
                            , [{pattern= FunApp (Operator Times, [x; beta]); _}]
                            )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, lub_mem)
                      , [y; x; Expr.Helpers.zero; beta] )
                | ( "poisson_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( Operator Plus
                            , [ alpha
                              ; {pattern= FunApp (Operator Times, [x; beta]); _}
                              ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, mem_type)
                      , [y; x; alpha; beta] )
                | ( "poisson_log_lpmf"
                  , [ y
                    ; { pattern=
                          FunApp
                            ( Operator Plus
                            , [ {pattern= FunApp (Operator Times, [x; beta]); _}
                              ; alpha ] )
                      ; _ } ] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, mem_type)
                      , [y; x; alpha; beta] )
                | ( "poisson_log_lpmf"
                  , [y; {pattern= FunApp (Operator Times, [x; beta]); _}] )
                  when Expr.Typed.type_of x = UMatrix ->
                    FunApp
                      ( StanLib ("poisson_log_glm_lpmf", suffix, mem_type)
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
                | "pow", [x; {pattern= FunApp (Operator Divide, [y; z]); _}]
                  when is_int 1 y && is_int 2 z
                       && not (y.meta.type_ = UInt && z.meta.type_ = UInt) ->
                    FunApp (StanLib ("sqrt", suffix, mem_type), [x])
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
                            ( StanLib ("square", FnPlain, mem)
                            , [{pattern= FunApp (Operator Minus, [x; y]); _}] )
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
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
                            ( Operator Times
                            , [ { pattern=
                                    FunApp
                                      ( Operator Times
                                      , [ { pattern=
                                              FunApp
                                                ( Operator Times
                                                , [ d
                                                  ; { pattern=
                                                        FunApp
                                                          ( ( Operator Transpose
                                                            | StanLib
                                                                ( "transpose"
                                                                , _
                                                                , _ ) )
                                                          , [b] )
                                                    ; _ } ] )
                                          ; _ }; a ] )
                                ; _ }; c ] )
                      ; _ } ] )
                  when Expr.Typed.equal b c ->
                    FunApp
                      ( StanLib ("trace_gen_quad_form", suffix, mem_type)
                      , [d; a; b] )
                | ( "trace"
                  , [ { pattern=
                          FunApp (StanLib ("quad_form", FnPlain, mem), [a; b])
                      ; _ } ] ) ->
                    let lub_mem = lub_mem_pat [mem] in
                    FunApp (StanLib ("trace_quad_form", suffix, lub_mem), [a; b])
                | _ -> FunApp (kind, l)))
      | TernaryIf (e1, e2, e3) -> (
          match eval_expr ~preserve_stability e1 with
          | x when is_int 0 x -> (eval_expr ~preserve_stability e3).pattern
          | {pattern= Lit (Int, _); _} ->
              (eval_expr ~preserve_stability e2).pattern
          | e1' ->
              TernaryIf
                ( e1'
                , try_eval_expr ~preserve_stability e2
                , try_eval_expr ~preserve_stability e3 ))
      | EAnd (e1, e2) -> (
          match
            ( eval_expr ~preserve_stability e1
            , try_eval_expr ~preserve_stability e2 )
          with
          | x, _ when is_int 0 x -> x.pattern
          | {pattern= Lit (Int, s1); _}, {pattern= Lit (Int, s2); _} ->
              let i1, i2 = (Int.of_string s1, Int.of_string s2) in
              Lit (Int, Int.to_string (Bool.to_int (i1 <> 0 && i2 <> 0)))
          | {pattern= Lit (_, s1); _}, {pattern= Lit (_, s2); _} ->
              let r1, r2 = (Float.of_string s1, Float.of_string s2) in
              Lit (Int, Int.to_string (Bool.to_int (r1 <> 0. && r2 <> 0.)))
          | e1', e2' -> EAnd (e1', e2'))
      | EOr (e1, e2) -> (
          match
            ( eval_expr ~preserve_stability e1
            , try_eval_expr ~preserve_stability e2 )
          with
          | x, _ when is_int 1 x -> x.pattern
          | {pattern= Lit (Int, s1); _}, {pattern= Lit (Int, s2); _} ->
              let i1, i2 = (Int.of_string s1, Int.of_string s2) in
              Lit (Int, Int.to_string (Bool.to_int (i1 <> 0 || i2 <> 0)))
          | {pattern= Lit (_, s1); _}, {pattern= Lit (_, s2); _} ->
              let r1, r2 = (Float.of_string s1, Float.of_string s2) in
              Lit (Int, Int.to_string (Bool.to_int (r1 <> 0. || r2 <> 0.)))
          | e1', e2' -> EOr (e1', e2'))
      | TupleProjection
          ({pattern= FunApp (CompilerInternal FnMakeTuple, ts); _}, ix) ->
          (List.nth ts (ix - 1)).pattern
      | TupleProjection (e, ix) -> TupleProjection (eval_expr e, ix)
      | Indexed (e, l) ->
          (* TODO: do something clever with array and matrix expressions here?
             Note that we could also constant fold array sizes if we keep those
             around on declarations. *)
          Indexed (eval_expr e, List.map ~f:(Index.map eval_expr) l)) }

and try_eval_expr ?(preserve_stability = false) expr =
  try eval_expr ~preserve_stability expr with Rejected _ -> expr

let rec simplify_index_expr pattern =
  Expr.(
    match pattern with
    | Pattern.Indexed
        ( {pattern= Indexed (obj, inner_indices); meta}
        , (Single ({meta= Expr.Typed.Meta.{type_= UInt; _}; _} as single_e) as
           single)
          :: outer_tl )
      when List.exists ~f:is_multi_index inner_indices -> (
        let idx =
          List.find_index ~f:is_multi_index inner_indices
          |> Option.value ~default:(List.length inner_indices) in
        match List.split_n inner_indices idx with
        | inner_singles, MultiIndex first_multi :: inner_tl ->
            (* foo [arr1, ..., arrN] [i1, ..., iN] ->
             * foo [arr1[i1]] [arr[i2]] ... [arrN[iN]]
             *)
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
            Common.ICE.(
              let pp = Fmt.list (Index.pp Expr.Typed.pp) in
              internal_errorf
                "There must be a multi-index. singles %t multis %t "
                [pp $ inner_singles; pp $ multis]) [@coverage off])
    | e -> e)

let expand_indices name known_sizes indices =
  (match String.Map.find_opt name known_sizes with
    | None -> indices
    | Some s ->
        List.mapi indices ~f:(fun i idx ->
            match (Array.get s i, idx) with
            | Some s, Index.Between (l, u) when Expr.Typed.equal s u ->
                Index.Upfrom l
            | _ -> idx))
  |> List.map ~f:(function
    | Index.Upfrom n when Expr.Typed.equal n Expr.Helpers.loop_bottom ->
        Index.All
    | i -> i)

let expand_indices_expr known_sizes = function
  | Expr.Pattern.Indexed (({Expr.pattern= Var name; _} as obj), indices) ->
      Expr.Pattern.Indexed (obj, expand_indices name known_sizes indices)
  | e -> e

let remove_trailing_alls_expr = function
  | Expr.Pattern.Indexed (obj, indices) ->
      (* a[2][:] -> a[2] *)
      let rec remove_trailing_alls indices =
        match List.rev indices with
        | Index.All :: tl -> remove_trailing_alls (List.rev tl)
        | _ -> indices in
      Expr.Pattern.Indexed (obj, remove_trailing_alls indices)
  | e -> e

let rec simplify_indices_expr known_sizes expr =
  Expr.(
    let pattern =
      expr.pattern
      |> expand_indices_expr known_sizes
      |> remove_trailing_alls_expr |> simplify_index_expr
      |> Expr.Pattern.map (simplify_indices_expr known_sizes) in
    {expr with pattern})

type declsize_info =
  { known_sizes: Expr.Typed.t option Array.t String.Map.t
  ; deps: (string * int) list String.Map.t }

let empty_sizes = {known_sizes= String.Map.empty; deps= String.Map.empty}

let copy_context (info : declsize_info) : declsize_info =
  {info with known_sizes= String.Map.map info.known_sizes ~f:Array.copy}

let add_known_size (info : declsize_info) (name : string)
    (sizes : Expr.Typed.t list) : declsize_info =
  let sizes =
    sizes
    |> List.map ~f:(fun d ->
        if Mir_utils.cannot_duplicate_expr d then None else Some d) in
  if List.exists ~f:Option.is_some sizes then
    let deps =
      List.filter_mapi sizes ~f:(fun i -> function
        | None -> None
        | Some d -> Some (i, Mir_utils.expr_var_names_set d))
      |> List.fold_left ~init:info.deps ~f:(fun init (i, names) ->
          Set.Poly.fold names ~init ~f:(fun key ->
              String.Map.update ~key ~f:(fun l ->
                  Some ((name, i) :: Option.value l ~default:[])))) in
    { known_sizes=
        String.Map.add info.known_sizes ~key:name ~data:(Array.of_list sizes)
    ; deps }
  else info

let erase_lval (info : declsize_info) lval =
  { info with
    deps=
      String.Map.update info.deps ~key:(Stmt.Helpers.lhs_variable lval)
        ~f:(fun l ->
          Option.iter l ~f:(fun l ->
              List.iter l ~f:(fun (name, i) ->
                  Option.iter (String.Map.find_opt name info.known_sizes)
                    ~f:(fun a -> Array.set a i None)));
          None) }

let rec erase_stmt (info : declsize_info) = function
  | {Stmt.pattern= Assignment (lval, _, _); _} -> erase_lval info lval
  | {pattern; _} -> Stmt.Pattern.fold Fun.const erase_stmt info pattern

let rec eval_stmt info Stmt.{pattern; meta} =
  let expr = Fun.compose eval_expr (simplify_indices_expr info.known_sizes) in
  let stmts init = List.fold_left_map ~init ~f:eval_stmt in
  let swrap pattern = Stmt.{pattern; meta} in
  try
    match pattern with
    | Decl {decl_adtype; decl_id; decl_type= Type.Sized st; initialize} ->
        let st = SizedType.map expr st in
        let initialize = Stmt.Pattern.map_decl_init expr initialize in
        ( add_known_size info decl_id (SizedType.get_dims st)
        , Decl {decl_adtype; decl_id; decl_type= Type.Sized st; initialize}
          |> swrap )
    | Assignment ((LVariable name, indices), type_, value) ->
        let indices =
          List.map indices ~f:(Index.map expr)
          |> expand_indices name info.known_sizes in
        let lval = (Stmt.Pattern.LVariable name, indices) in
        let value = expr value in
        (erase_lval info lval, Assignment (lval, type_, value) |> swrap)
    | Assignment (lval, type_, value) ->
        let lval = Stmt.Pattern.map_lvalue expr lval in
        let value = expr value in
        (erase_lval info lval, Assignment (lval, type_, value) |> swrap)
    | SList s ->
        let info, s = stmts info s in
        (info, SList s |> swrap)
    | Block s ->
        let _, s = stmts info s in
        (info, Block s |> swrap)
    | Profile (name, s) ->
        let _, s = stmts info s in
        (info, Profile (name, s) |> swrap)
    | For {loopvar; lower; upper; body} ->
        let lower = expr lower in
        let info = erase_stmt info body in
        let upper = expr upper in
        let info, body = eval_stmt info body in
        (info, For {loopvar; lower; upper; body} |> swrap)
    | While (cond, body) ->
        let info, body = eval_stmt (erase_stmt info body) body in
        (info, While (expr cond, body) |> swrap)
    | IfElse (cond, thn, None) ->
        let cond = expr cond in
        let info, thn = eval_stmt info thn in
        (info, IfElse (cond, thn, None) |> swrap)
    | IfElse (cond, thn, Some els) ->
        let cond = expr cond in
        let _, thn = eval_stmt (copy_context info) thn in
        let info, els = eval_stmt info els in
        (erase_stmt info thn, IfElse (cond, thn, Some els) |> swrap)
    | other -> (info, Stmt.Pattern.map expr Fun.id other |> swrap)
  with Rejected (loc, m) ->
    ( info
    , { pattern= NRFunApp (CompilerInternal FnReject, [Expr.Helpers.str m])
      ; meta= loc } )

let eval_stmt s = eval_stmt empty_sizes s |> snd
