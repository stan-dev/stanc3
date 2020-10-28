open Core_kernel
open Ast
open Middle

let deprecated_functions =
  String.Map.of_alist_exn
    [ ("multiply_log", "lmultiply")
    ; ("binomial_coefficient_log", "lchoose")
    ; ("integrate_ode", "integrate_ode_rk45")
    ; ("cov_exp_quad", "gp_cov_exp_quad") ]

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

let deprecated_userdefined : string String.Table.t = String.Table.create ()

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
  Fmt.pf Fmt.stderr
    "@[<v>@,Warning: deprecated language construct used in %s:@,%s@]@."
    (Location_span.to_string loc_span)
    message

let replace_suffix = function
  | { stmt= FunDef {funname= {name; _}; arguments= (_, type_, _) :: _; _}
    ; smeta= _ }
    when String.is_suffix ~suffix:"_log" name ->
      let open String in
      let newname =
        if is_suffix ~suffix:"_cdf_log" name then drop_suffix name 8 ^ "_lcdf"
        else if is_suffix ~suffix:"_ccdf_log" name then
          drop_suffix name 9 ^ "_lccdf"
        else if Middle.UnsizedType.is_real_type type_ then
          drop_suffix name 4 ^ "_lpdf"
        else drop_suffix name 4 ^ "_lpmf"
      in
      Table.add deprecated_userdefined ~key:name ~data:newname
      |> (ignore : [`Ok | `Duplicate] -> unit)
  | _ -> ()

let rec warn_deprecated_expr
    ({expr; emeta} : (typed_expr_meta, fun_kind) expr_with) :
    (typed_expr_meta, fun_kind) expr_with =
  let expr =
    match expr with
    | GetLP ->
        warn_deprecated
          ( emeta.loc
          , "The no-argument function `get_lp()` is deprecated. Use the \
             no-argument function `target()` instead." ) ;
        GetLP
    | FunApp (StanLib, {name= "abs"; id_loc}, [e])
      when Middle.UnsizedType.is_real_type e.emeta.type_ ->
        warn_deprecated
          ( emeta.loc
          , "Use of the `abs` function with real-valued arguments is \
             deprecated; use functions `fabs` instead." ) ;
        FunApp (StanLib, {name= "abs"; id_loc}, [warn_deprecated_expr e])
    | FunApp (StanLib, {name= "if_else"; id_loc}, l) ->
        warn_deprecated
          ( emeta.loc
          , "The function `if_else` is deprecated. Use the conditional \
             operator (x ? y : z) instead." ) ;
        FunApp
          ( StanLib
          , {name= "if_else"; id_loc}
          , List.map ~f:warn_deprecated_expr l )
    | FunApp (StanLib, {name; id_loc}, l) ->
        if Option.is_some (String.Map.find deprecated_distributions name) then
          warn_deprecated
            ( emeta.loc
            , name ^ " is deprecated, use " ^ rename_distribution name
              ^ " instead" )
        else if Option.is_some (String.Map.find deprecated_functions name) then
          warn_deprecated
            ( emeta.loc
            , name ^ " is deprecated, use " ^ rename_function name ^ " instead"
            ) ;
        FunApp (StanLib, {name; id_loc}, List.map ~f:warn_deprecated_expr l)
    | FunApp (UserDefined, {name; id_loc}, l) ->
        let newname = String.Table.find deprecated_userdefined name in
        if Option.is_some newname then
          warn_deprecated
            ( emeta.loc
            , "Use of the _log suffix in user defined function " ^ name
              ^ " is deprecated, use " ^ Option.value_exn newname ^ " instead."
            ) ;
        FunApp (UserDefined, {name; id_loc}, List.map ~f:warn_deprecated_expr l)
    | _ -> map_expression warn_deprecated_expr ident expr
  in
  {expr; emeta}

let warn_deprecated_lval = map_lval_with warn_deprecated_expr ident

let rec warn_deprecated_stmt {stmt; smeta} =
  let stmt =
    match stmt with
    | IncrementLogProb e -> IncrementLogProb (warn_deprecated_expr e)
    | Assignment {assign_lhs= l; assign_op= ArrowAssign; assign_rhs= e} ->
        Assignment
          { assign_lhs= warn_deprecated_lval l
          ; assign_op= ArrowAssign
          ; assign_rhs= warn_deprecated_expr e }
    | FunDef {returntype; funname= {name; id_loc}; arguments; body} ->
        FunDef
          { returntype
          ; funname= {name; id_loc}
          ; arguments
          ; body= warn_deprecated_stmt body }
    | _ ->
        map_statement warn_deprecated_expr warn_deprecated_stmt
          warn_deprecated_lval ident stmt
  in
  {stmt; smeta}

let emit_warnings program : typed_program =
  String.Table.clear deprecated_userdefined ;
  program.functionblock |> Option.iter ~f:(List.iter ~f:replace_suffix) ;
  program |> map_program warn_deprecated_stmt
