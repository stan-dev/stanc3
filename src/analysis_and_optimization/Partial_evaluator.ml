(* A partial evaluator for use in static analysis and optimization *)

open Core_kernel
open Mir_utils
open Middle

let apply_prefix_operator_int (op : string) i =
  Lit
    ( Int
    , Int.to_string
        ( match op with
        | "PPlus__" -> i
        | "PMinus__" -> -i
        | "PNot__" -> if i = 0 then 1 else 0
        | s -> raise_s [%sexp (s : string)] ) )

let apply_prefix_operator_real (op : string) i =
  Lit
    ( Real
    , Float.to_string
        ( match op with
        | "PPlus__" -> i
        | "PMinus__" -> -.i
        | s -> raise_s [%sexp (s : string)] ) )

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
        | "Equals__" -> Bool.to_int (i1 = i2)
        | "NEquals__" -> Bool.to_int (i1 <> i2)
        | "Less__" -> Bool.to_int (i1 < i2)
        | "Leq__" -> Bool.to_int (i1 <= i2)
        | "Greater__" -> Bool.to_int (i1 > i2)
        | "Geq__" -> Bool.to_int (i1 >= i2)
        | s -> raise_s [%sexp (s : string)] ) )

let apply_arithmetic_operator_real (op : string) r1 r2 =
  Lit
    ( Real
    , Float.to_string
        ( match op with
        | "Plus__" -> r1 +. r2
        | "Minus__" -> r1 -. r2
        | "Times__" -> r1 *. r2
        | "Divide__" -> r1 /. r2
        | s -> raise_s [%sexp (s : string)] ) )

let apply_logical_operator_real (op : string) r1 r2 =
  Lit
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

let get_fun_or_op_rt_opt name expr =
  let argument_types =
    List.map ~f:(fun x -> Expr.Typed.(adlevel_of x,type_of x)) expr
  in
  match Operator.of_string_opt name with 
  | Some op -> Stan_math.op_return_type op argument_types
  | _ -> Stan_math.return_type name argument_types
            
let try_partially_evaluate_to f l e =
  match e with
  | Expr.Fixed.Pattern.FunApp (StanLib, fn_name, args) -> (
    match get_fun_or_op_rt_opt fn_name args with
    | Some _ -> Expr.Fixed.Pattern.FunApp (StanLib, fn_name, args)
    | None -> FunApp (StanLib, f, l) )
  | e -> e




(* TODO: deal with tilde statements and unnormalized distributions properly here *)
let simplify_dist meta name args = 
  match name , args with 
  | "bernoulli_lpmf", [y;theta] -> Some (Expr.Bernoulli.lpmf meta y theta)
  | "bernoulli_rng", [theta] -> Some (Expr.Bernoulli.rng meta theta)
  | "bernoulli_logit_lpmf", [y;theta] -> Some (Expr.Bernoulli_logit.lpmf meta y theta)
  | "binomial_lpmf", [y;n;theta] -> Some (Expr.Binomial.lpmf meta y n theta)
  | "categorical_lpmf", [y;theta] -> Some (Expr.Categorical.lpmf meta y theta)
  | "categorical_rng", [theta] -> Some (Expr.Categorical.rng meta theta)
  | "neg_binomial_2_lpmf", [n;location;precision] -> Some(Expr.Neg_binomial_2.lpmf meta n location precision)
  | "neg_binomial_2_rng", [location;precision] -> Some(Expr.Neg_binomial_2.rng meta location precision)
  | "neg_binomial_2_log_lpmf",[log_loc;precision] ->  Some(Expr.Neg_binomial_2_log.rng meta log_loc precision)
  | "poisson_lpmf",[y;lambda] -> Some(Expr.Poisson.lpmf meta y lambda)
  | "poisson_rng",[lambda] -> Some(Expr.Poisson.rng meta  lambda)
  | "poisson_log_lpmf",[y;alpha] -> Some(Expr.Poisson_log.lpmf meta y alpha)
  | "multi_normal_lpdf",[y;me;sigma] -> Some(Expr.Multi_normal.lpmf meta y mu sigma)

  | _ -> None
  




            





            | "columns_dot_product", [x; y]
              when compare_expr_typed_located x y = 0 ->
                FunApp (StanLib, "columns_dot_self", [x])


            | "dot_product", [x; y] when compare_expr_typed_located x y = 0 ->
                FunApp (StanLib, "dot_self", [x])

            | "rows_dot_product", [x; y]
              when compare_expr_typed_located x y = 0 ->
                FunApp (StanLib, "rows_dot_self", [x])




                (* Constant folding for operators *)
            | op, [{expr= Lit (Int, i); _}] -> (
              match op with
              | "PPlus_" | "PMinus__" | "PNot__" ->
                  apply_prefix_operator_int op (Int.of_string i)
              | _ -> FunApp (StanLib, op, l) )

            | op, [{expr= Lit (Real, r); _}] -> (
              match op with
              | "PPlus_" | "PMinus__" ->
                  apply_prefix_operator_real op (Float.of_string r)
              | _ -> FunApp (StanLib, op, l) )

            | op, [{expr= Lit (Int, i1); _}; {expr= Lit (Int, i2); _}] -> (
              match op with
              | "Plus__" | "Minus__" | "Times__" | "Divide__" | "Modulo__"
               |"Or__" | "And__" | "Equals__" | "NEquals__" | "Less__"
               |"Leq__" | "Greater__" | "Geq__" ->
                  apply_operator_int op (Int.of_string i1) (Int.of_string i2)
              | _ -> FunApp (StanLib, op, l) )

            | op, [{expr= Lit (Real, i1); _}; {expr= Lit (Real, i2); _}]
             |op, [{expr= Lit (Int, i1); _}; {expr= Lit (Real, i2); _}]
             |op, [{expr= Lit (Real, i1); _}; {expr= Lit (Int, i2); _}] -> (
              match op with
              | "Plus__" | "Minus__" | "Times__" | "Divide__" ->
                  apply_arithmetic_operator_real op (Float.of_string i1)
                    (Float.of_string i2)
              | "Or__" | "And__" | "Equals__" | "NEquals__" | "Less__"
               |"Leq__" | "Greater__" | "Geq__" ->
                  apply_logical_operator_real op (Float.of_string i1)
                    (Float.of_string i2)
              | _ -> FunApp (StanLib, op, l) )
            | _ -> FunApp (t, f, l) )

let rec eval_expr expr =
  match Expr.proj expr with 
  | _ , Var _ | _ , Lit _ -> expr 
  | _ , FunApp(t,f,l) -> 
      let l = List.map ~f:eval_expr l in
          
          try_partially_evaluate_to f l

  { e with
    expr=
      ( match e.expr with
      | Var _ | Lit (_, _) -> e.expr
      | FunApp (t, f, l) ->
          let l = List.map ~f:eval_expr l in
          
          try_partially_evaluate_to f l
            
      | TernaryIf (e1, e2, e3) -> (
        match (eval_expr e1, eval_expr e2, eval_expr e3) with
        | {expr= Lit (Int, "0"); _}, _, e3' -> e3'.expr
        | {expr= Lit (Int, _); _}, e2', _ -> e2'.expr
        | e1', e2', e3' -> TernaryIf (e1', e2', e3') )
      | EAnd (e1, e2) -> (
        match (eval_expr e1, eval_expr e2) with
        | {expr= Lit (Int, s1); _}, {expr= Lit (Int, s2); _} ->
            let i1, i2 = (Int.of_string s1, Int.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (i1 <> 0 && i2 <> 0)))
        | {expr= Lit (Real, s1); _}, {expr= Lit (Real, s2); _} ->
            let r1, r2 = (Float.of_string s1, Float.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (r1 <> 0. && r2 <> 0.)))
        | e1', e2' -> EAnd (e1', e2') )
      | EOr (e1, e2) -> (
        match (eval_expr e1, eval_expr e2) with
        | {expr= Lit (Int, s1); _}, {expr= Lit (Int, s2); _} ->
            let i1, i2 = (Int.of_string s1, Int.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (i1 <> 0 || i2 <> 0)))
        | {expr= Lit (Real, s1); _}, {expr= Lit (Real, s2); _} ->
            let r1, r2 = (Float.of_string s1, Float.of_string s2) in
            Lit (Int, Int.to_string (Bool.to_int (r1 <> 0. || r2 <> 0.)))
        | e1', e2' -> EOr (e1', e2') )
      | Indexed (e, l) ->
          (* TODO: do something clever with array and matrix expressions here?
  Note  that we could also constant fold array sizes if we keep those around on declarations. *)
          Indexed (eval_expr e, List.map ~f:eval_idx l) ) }

and eval_idx i = map_index eval_expr i

let eval_stmt_base = map_statement eval_expr (fun x -> x)
let eval_stmt = map_rec_stmt_loc eval_stmt_base
let eval_prog = map_prog eval_expr eval_stmt
