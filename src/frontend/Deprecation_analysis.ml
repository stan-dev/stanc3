open Core_kernel
open Ast
open Middle

module type Deprecation_analizer = sig
  val find_udf_log_suffix :
    typed_statement -> (string * Middle.UnsizedType.t) option

  val update_suffix : string -> Middle.UnsizedType.t -> string

  val collect_userdef_distributions :
    typed_program -> Middle.UnsizedType.t String.Map.t

  val distribution_suffix : string -> bool
  val without_suffix : string list -> string -> string
  val is_deprecated_distribution : string -> bool
  val rename_deprecated_distribution : string -> string
  val rename_deprecated_function : string -> string
  val userdef_distributions : untyped_statement block option -> string list
  val collect_warnings : typed_program -> Warnings.t list
end

module Make (StdLib : Std_library_utils.Library) : Deprecation_analizer = struct
  (* String.Map.of_alist_exn
     (List.map
        ~f:(fun (x, y) -> (x, (y, "2.32.0")))
        (List.concat_map StdLib.distributions
           ~f:(fun (fnkinds, name, _, _) ->
             List.filter_map fnkinds ~f:(function
               | Lpdf -> Some (name ^ "_log", name ^ "_lpdf")
               | Lpmf -> Some (name ^ "_log", name ^ "_lpmf")
               | Cdf -> Some (name ^ "_cdf_log", name ^ "_lcdf")
               | Ccdf -> Some (name ^ "_ccdf_log", name ^ "_lccdf")
               | Rng | UnaryVectorized -> None ) ) ) ) *)
  (* String.Map.of_alist_exn
          [ ("multiply_log", ("lmultiply", "2.32.0"))
          ; ("binomial_coefficient_log", ("lchoose", "2.32.0"))
          ; ("cov_exp_quad", ("gp_exp_quad_cov", "2.32.0")) ]

          +
     This can be automatically changed using the \
                         canonicalize flag for stanc
  *)

  (* String.Map.of_alist_exn
      [ ("integrate_ode", ("ode_rk45", "3.0"))
      ; ("integrate_ode_rk45", ("ode_rk45", "3.0"))
      ; ("integrate_ode_bdf", ("ode_bdf", "3.0"))
      ; ("integrate_ode_adams", ("ode_adams", "3.0")) ]

      +
      The new interface is slightly different, see:
     https://mc-stan.org/users/documentation/case-studies/convert_odes.html
  *)

  let stan_lib_deprecations =
    Map.merge_skewed StdLib.deprecated_distributions StdLib.deprecated_functions
      ~combine:(fun ~key x y ->
        Common.FatalError.fatal_error_msg
          [%message
            "Common key in deprecation map"
              (key : string)
              (x : Std_library_utils.deprecation_info)
              (y : Std_library_utils.deprecation_info)] )

  let is_deprecated_distribution name =
    Map.mem StdLib.deprecated_distributions name

  let rename_deprecated map name =
    Map.find map name
    |> Option.map ~f:(fun Std_library_utils.{replacement; _} -> replacement)
    |> Option.value ~default:name

  let rename_deprecated_distribution =
    rename_deprecated StdLib.deprecated_distributions

  let rename_deprecated_function = rename_deprecated StdLib.deprecated_functions

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
        | _ -> None )
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
    else if is_suffix ~suffix:"_ccdf_log" name then
      drop_suffix name 9 ^ "_lccdf"
    else if Middle.UnsizedType.is_int_type type_ then
      drop_suffix name 4 ^ "_lpmf"
    else drop_suffix name 4 ^ "_lpdf"

  let find_udf_log_suffix = function
    | { stmt=
          FunDef
            { funname= {name; _}
            ; arguments= (_, ((UReal | UInt) as type_), _) :: _
            ; _ }
      ; smeta= _ }
      when String.is_suffix ~suffix:"_log" name ->
        Some (name, type_)
    | _ -> None

  let rec collect_deprecated_expr (acc : (Location_span.t * string) list)
      ({expr; emeta} : (typed_expr_meta, fun_kind) expr_with) :
      (Location_span.t * string) list =
    match expr with
    | FunApp (StanLib FnPlain, {name= "abs"; _}, [e])
      when Middle.UnsizedType.is_real_type e.emeta.type_ ->
        collect_deprecated_expr
          ( acc
          @ [ ( emeta.loc
              , "Use of the `abs` function with real-valued arguments is \
                 deprecated; use function `fabs` instead." ) ] )
          e
    | FunApp (StanLib FnPlain, {name= "if_else"; _}, l) ->
        acc
        @ [ ( emeta.loc
            , "The function `if_else` is deprecated and will be removed in \
               Stan 2.32.0. Use the conditional operator (x ? y : z) instead; \
               this can be automatically changed using the canonicalize flag \
               for stanc" ) ]
        @ List.concat_map l ~f:(fun e -> collect_deprecated_expr [] e)
    | FunApp ((StanLib _ | UserDefined _), {name; _}, l) ->
        let w =
          match Map.find stan_lib_deprecations name with
          | Some {replacement; version; extra_message} ->
              [ ( emeta.loc
                , name ^ " is deprecated and will be removed in Stan " ^ version
                  ^ ". Use " ^ replacement ^ " instead. " ^ extra_message ) ]
          | _ when String.is_suffix name ~suffix:"_cdf" ->
              [ ( emeta.loc
                , "Use of " ^ name
                  ^ " without a vertical bar (|) between the first two \
                     arguments of a CDF is deprecated and will be removed in \
                     Stan 2.32.0. This can be automatically changed using the \
                     canonicalize flag for stanc" ) ]
          | _ -> [] in
        acc @ w @ List.concat_map l ~f:(fun e -> collect_deprecated_expr [] e)
    | _ -> fold_expression collect_deprecated_expr (fun l _ -> l) acc expr

  let collect_deprecated_lval acc l =
    fold_lval_with collect_deprecated_expr (fun x _ -> x) acc l

  let rec collect_deprecated_stmt (acc : (Location_span.t * string) list)
      {stmt; _} : (Location_span.t * string) list =
    match stmt with
    | FunDef
        { body
        ; funname= {name; id_loc}
        ; arguments= (_, ((UReal | UInt) as type_), _) :: _
        ; _ }
      when String.is_suffix ~suffix:"_log" name ->
        let acc =
          acc
          @ [ ( id_loc
              , "Use of the _log suffix in user defined probability functions \
                 is deprecated and will be removed in Stan 2.32.0, use name '"
                ^ update_suffix name type_
                ^ "' instead if you intend on using this function in ~ \
                   statements or calling unnormalized probability functions \
                   inside of it." ) ] in
        collect_deprecated_stmt acc body
    | FunDef {body; _} -> collect_deprecated_stmt acc body
    | _ ->
        fold_statement collect_deprecated_expr collect_deprecated_stmt
          collect_deprecated_lval
          (fun l _ -> l)
          acc stmt

  let collect_userdef_distributions program =
    program.functionblock |> Ast.get_stmts
    |> List.filter_map ~f:find_udf_log_suffix
    |> List.dedup_and_sort ~compare:(fun (x, _) (y, _) -> String.compare x y)
    |> String.Map.of_alist_exn

  let collect_warnings (program : typed_program) =
    fold_program collect_deprecated_stmt [] program
end
