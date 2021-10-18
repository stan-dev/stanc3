open Core_kernel
open Ast
open Middle

type t = Location_span.t * string

let deprecated_functions =
  String.Map.of_alist_exn
    [ ("multiply_log", "lmultiply")
    ; ("binomial_coefficient_log", "lchoose")
    ; ("cov_exp_quad", "gp_exp_quad_cov") ]

let deprecated_odes =
  String.Map.of_alist_exn
    [ ("integrate_ode", "ode_rk45")
    ; ("integrate_ode_rk45", "ode_rk45")
    ; ("integrate_ode_bdf", "ode_bdf")
    ; ("integrate_ode_adams", "ode_adams") ]

let deprecated_distributions =
  String.Map.of_alist_exn
    (List.concat_map Middle.Stan_math_signatures.distributions
       ~f:(fun (fnkinds, name, _, _) ->
         List.filter_map fnkinds ~f:(function
           | Lpdf -> Some (name ^ "_log", name ^ "_lpdf")
           | Lpmf -> Some (name ^ "_log", name ^ "_lpmf")
           | Cdf -> Some (name ^ "_cdf_log", name ^ "_lcdf")
           | Ccdf -> Some (name ^ "_ccdf_log", name ^ "_lccdf")
           | Rng | UnaryVectorized -> None ) ))

let is_deprecated_distribution name =
  Option.is_some (String.Map.find deprecated_distributions name)

let rename_deprecated map name =
  Option.value ~default:name (String.Map.find map name)

let distribution_suffix name =
  let open String in
  is_suffix ~suffix:"_lpdf" name
  || is_suffix ~suffix:"_lpmf" name
  || is_suffix ~suffix:"_lcdf" name
  || is_suffix ~suffix:"_lccdf" name

let userdef_distributions stmts =
  let open String in
  List.filter_map
    ~f:(function
      | {stmt= FunDef {funname= {name; _}; _}; _} ->
          if
            is_suffix ~suffix:"_log_lpdf" name
            || is_suffix ~suffix:"_log_lpmf" name
          then Some (drop_suffix name 5)
          else if is_suffix ~suffix:"_log_log" name then
            Some (drop_suffix name 4)
          else None
      | _ -> None)
    (Ast.get_stmts stmts)

let without_suffix user_dists name =
  let open String in
  if is_suffix ~suffix:"_lpdf" name || is_suffix ~suffix:"_lpmf" name then
    drop_suffix name 5
  else if
    is_suffix ~suffix:"_log" name
    && not
         ( is_deprecated_distribution (name ^ "_log")
         || List.exists ~f:(( = ) name) user_dists )
  then drop_suffix name 4
  else name

let update_suffix name type_ =
  let open String in
  if is_suffix ~suffix:"_cdf_log" name then drop_suffix name 8 ^ "_lcdf"
  else if is_suffix ~suffix:"_ccdf_log" name then drop_suffix name 9 ^ "_lccdf"
  else if Middle.UnsizedType.is_real_type type_ then
    drop_suffix name 4 ^ "_lpdf"
  else drop_suffix name 4 ^ "_lpmf"

let find_udf_log_suffix = function
  | { stmt= FunDef {funname= {name; _}; arguments= (_, type_, _) :: _; _}
    ; smeta= _ }
    when String.is_suffix ~suffix:"_log" name ->
      Some (name, type_)
  | _ -> None

let rec collect_deprecated_expr deprecated_userdefined
    (acc : (Location_span.t * string) list)
    ({expr; emeta} : (typed_expr_meta, fun_kind) expr_with) :
    (Location_span.t * string) list =
  match expr with
  | GetLP ->
      acc
      @ [ ( emeta.loc
          , "The no-argument function `get_lp()` is deprecated. Use the \
             no-argument function `target()` instead." ) ]
  | FunApp (StanLib FnPlain, {name= "abs"; _}, [e])
    when Middle.UnsizedType.is_real_type e.emeta.type_ ->
      collect_deprecated_expr deprecated_userdefined
        ( acc
        @ [ ( emeta.loc
            , "Use of the `abs` function with real-valued arguments is \
               deprecated; use functions `fabs` instead." ) ] )
        e
  | FunApp (StanLib FnPlain, {name= "if_else"; _}, l) ->
      acc
      @ [ ( emeta.loc
          , "The function `if_else` is deprecated. Use the conditional \
             operator (x ? y : z) instead." ) ]
      @ List.concat
          (List.map l ~f:(fun e ->
               collect_deprecated_expr deprecated_userdefined [] e ))
  | FunApp (StanLib _, {name; _}, l) ->
      let w =
        if Option.is_some (String.Map.find deprecated_distributions name) then
          [ ( emeta.loc
            , name ^ " is deprecated and will be removed in the future. Use "
              ^ rename_deprecated deprecated_distributions name
              ^ " instead." ) ]
        else if String.is_suffix name ~suffix:"_cdf" then
          [ ( emeta.loc
            , "Use of " ^ name
              ^ " without a vertical bar (|) between the first two arguments \
                 is deprecated." ) ]
        else if Option.is_some (String.Map.find deprecated_functions name) then
          [ ( emeta.loc
            , name ^ " is deprecated and will be removed in the future. Use "
              ^ rename_deprecated deprecated_functions name
              ^ " instead." ) ]
        else if Option.is_some (String.Map.find deprecated_odes name) then
          [ ( emeta.loc
            , name ^ " is deprecated and will be removed in the future. Use "
              ^ rename_deprecated deprecated_odes name
              ^ " instead. \n\
                 The new interface is slightly different, see: \n\
                 https://mc-stan.org/users/documentation/case-studies/convert_odes.html"
            ) ]
        else []
      in
      acc @ w
      @ List.concat
          (List.map l ~f:(fun e ->
               collect_deprecated_expr deprecated_userdefined [] e ))
  | FunApp (UserDefined _, {name; _}, l) ->
      let w =
        let type_ = String.Map.find deprecated_userdefined name in
        if Option.is_some type_ then
          [ ( emeta.loc
            , "Use of the _log suffix in user defined function " ^ name
              ^ " is deprecated, use "
              ^ update_suffix name (Option.value_exn type_)
              ^ " instead." ) ]
        else if String.is_suffix name ~suffix:"_cdf" then
          [ ( emeta.loc
            , "Use of " ^ name
              ^ " without a vertical bar (|) between the first two arguments \
                 is deprecated." ) ]
        else []
      in
      acc @ w
      @ List.concat
          (List.map l ~f:(fun e ->
               collect_deprecated_expr deprecated_userdefined [] e ))
  | _ ->
      fold_expression
        (collect_deprecated_expr deprecated_userdefined)
        (fun l _ -> l)
        acc expr

let collect_deprecated_lval deprecated_userdefined acc l =
  fold_lval_with
    (collect_deprecated_expr deprecated_userdefined)
    (fun x _ -> x)
    acc l

let rec collect_deprecated_stmt deprecated_userdefined
    (acc : (Location_span.t * string) list) {stmt; _} :
    (Location_span.t * string) list =
  match stmt with
  | IncrementLogProb e -> collect_deprecated_expr deprecated_userdefined acc e
  | Assignment {assign_lhs= l; assign_op= ArrowAssign; assign_rhs= e} ->
      acc
      @ collect_deprecated_lval deprecated_userdefined [] l
      @ collect_deprecated_expr deprecated_userdefined [] e
  | FunDef {body; _} -> collect_deprecated_stmt deprecated_userdefined acc body
  | _ ->
      fold_statement
        (collect_deprecated_expr deprecated_userdefined)
        (collect_deprecated_stmt deprecated_userdefined)
        (collect_deprecated_lval deprecated_userdefined)
        (fun l _ -> l)
        acc stmt

let collect_userdef_distributions program =
  program.functionblock |> Ast.get_stmts
  |> List.filter_map ~f:find_udf_log_suffix
  |> List.dedup_and_sort ~compare:(fun (x, _) (y, _) -> String.compare x y)
  |> String.Map.of_alist_exn

let collect_warnings (program : typed_program) =
  fold_program
    (collect_deprecated_stmt (collect_userdef_distributions program))
    [] program
