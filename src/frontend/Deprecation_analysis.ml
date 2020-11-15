open Core_kernel
open Ast
open Middle

let deprecated_functions =
  String.Map.of_alist_exn
    [ ("multiply_log", "lmultiply")
    ; ("binomial_coefficient_log", "lchoose")
    ; ("cov_exp_quad", "gp_cov_exp_quad")
    ; ("integrate_ode", "ode_rk45")
    ; ("integrate_ode_rk45", "ode_rk45")
    ; ("integrate_ode_bdf", "ode_bdf")
    ; ("integrate_ode_adams", "ode_adams") ]

let deprecated_distributions =
  String.Map.of_alist_exn
    (List.concat_map Middle.Stan_math_signatures.distributions
       ~f:(fun (fnkinds, name, _) ->
         List.filter_map fnkinds ~f:(function
           | Lpdf -> Some (name ^ "_log", name ^ "_lpdf")
           | Lpmf -> Some (name ^ "_log", name ^ "_lpmf")
           | Cdf -> Some (name ^ "_cdf_log", name ^ "_lcdf")
           | Ccdf -> Some (name ^ "_ccdf_log", name ^ "_lccdf")
           | Rng | UnaryVectorized -> None ) ))

let is_distribution name =
  Option.is_some (String.Map.find deprecated_distributions name)

let rename_distribution name =
  Option.value ~default:name (String.Map.find deprecated_distributions name)

let rename_function name =
  Option.value ~default:name (String.Map.find deprecated_functions name)

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
    (Option.value ~default:[] stmts)

let without_suffix user_dists name =
  let open String in
  if is_suffix ~suffix:"_lpdf" name || is_suffix ~suffix:"_lpmf" name then
    drop_suffix name 5
  else if
    is_suffix ~suffix:"_log" name
    && not
         ( is_distribution (name ^ "_log")
         || List.exists ~f:(( = ) name) user_dists )
  then drop_suffix name 4
  else name

let warn_deprecated (loc_span, message) =
  Fmt.pf Fmt.stderr "@[<v>@,Warning: in %s: %s@]@."
    (Location_span.to_string loc_span)
    message

let update_suffix name type_ =
  let open String in
  if is_suffix ~suffix:"_cdf_log" name then drop_suffix name 8 ^ "_lcdf"
  else if is_suffix ~suffix:"_ccdf_log" name then drop_suffix name 9 ^ "_lccdf"
  else if Middle.UnsizedType.is_real_type type_ then
    drop_suffix name 4 ^ "_lpdf"
  else drop_suffix name 4 ^ "_lpmf"

let find_suffixes = function
  | { stmt= FunDef {funname= {name; _}; arguments= (_, type_, _) :: _; _}
    ; smeta= _ }
    when String.is_suffix ~suffix:"_log" name ->
      Some (name, type_)
  | _ -> None

let rec warn_deprecated_expr deprecated_userdefined
    (acc : (Location_span.t * string) list)
    ({expr; emeta} : (typed_expr_meta, fun_kind) expr_with) :
    (Location_span.t * string) list =
  match expr with
  | GetLP ->
      acc
      @ [ ( emeta.loc
          , "The no-argument function `get_lp()` is deprecated. Use the \
             no-argument function `target()` instead." ) ]
  | FunApp (StanLib, {name= "abs"; _}, [e])
    when Middle.UnsizedType.is_real_type e.emeta.type_ ->
      warn_deprecated_expr deprecated_userdefined
        ( acc
        @ [ ( emeta.loc
            , "Use of the `abs` function with real-valued arguments is \
               deprecated; use functions `fabs` instead." ) ] )
        e
  | FunApp (StanLib, {name= "if_else"; _}, l) ->
      acc
      @ [ ( emeta.loc
          , "The function `if_else` is deprecated. Use the conditional \
             operator (x ? y : z) instead." ) ]
      @ List.concat
          (List.map l ~f:(fun e ->
               warn_deprecated_expr deprecated_userdefined [] e ))
  | FunApp (StanLib, {name; _}, l) ->
      let w =
        if Option.is_some (String.Map.find deprecated_distributions name) then
          [ ( emeta.loc
            , name ^ " is deprecated and will be removed in the future. Use "
              ^ rename_distribution name ^ " instead." ) ]
        else if Option.is_some (String.Map.find deprecated_functions name) then
          [ ( emeta.loc
            , name ^ " is deprecated and will be removed in the future. Use "
              ^ rename_function name ^ " instead." ) ]
        else []
      in
      acc @ w
      @ List.concat
          (List.map l ~f:(fun e ->
               warn_deprecated_expr deprecated_userdefined [] e ))
  | FunApp (UserDefined, {name; _}, l) ->
      let w =
        let type_ = String.Map.find deprecated_userdefined name in
        if Option.is_some type_ then
          [ ( emeta.loc
            , "Use of the _log suffix in user defined function " ^ name
              ^ " is deprecated, use "
              ^ update_suffix name (Option.value_exn type_)
              ^ " instead." ) ]
        else []
      in
      acc @ w
      @ List.concat
          (List.map l ~f:(fun e ->
               warn_deprecated_expr deprecated_userdefined [] e ))
  | _ ->
      fold_expression
        (warn_deprecated_expr deprecated_userdefined)
        (fun l _ -> l)
        acc expr

let warn_deprecated_lval deprecated_userdefined acc l =
  fold_lval_with
    (warn_deprecated_expr deprecated_userdefined)
    (fun x _ -> x)
    acc l

let rec warn_deprecated_stmt deprecated_userdefined
    (acc : (Location_span.t * string) list) {stmt; _} :
    (Location_span.t * string) list =
  match stmt with
  | IncrementLogProb e -> warn_deprecated_expr deprecated_userdefined acc e
  | Assignment {assign_lhs= l; assign_op= ArrowAssign; assign_rhs= e} ->
      acc
      @ warn_deprecated_lval deprecated_userdefined [] l
      @ warn_deprecated_expr deprecated_userdefined [] e
  | FunDef {body; _} -> warn_deprecated_stmt deprecated_userdefined acc body
  | _ ->
      fold_statement
        (warn_deprecated_expr deprecated_userdefined)
        (warn_deprecated_stmt deprecated_userdefined)
        (warn_deprecated_lval deprecated_userdefined)
        (fun l _ -> l)
        acc stmt

let emit_warnings (program : typed_program) : unit =
  let deprecated_userdefined =
    program.functionblock |> Option.value ~default:[]
    |> List.filter_map ~f:find_suffixes
    |> List.dedup_and_sort ~compare:(fun (x, _) (y, _) -> String.compare x y)
    |> String.Map.of_alist_exn
  in
  fold_program (warn_deprecated_stmt deprecated_userdefined) [] program
  |> List.iter ~f:warn_deprecated
