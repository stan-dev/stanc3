open Core_kernel
open Std_library_utils

(** Many of the required functions are exposed in the backend specific file, so we include it *)
include Stan_math_backend.Stan_math_signatures

let deprecated_distributions =
  List.concat_map distributions ~f:(fun (fnkinds, name, _, _) ->
      List.filter_map fnkinds ~f:(function
        | Lpdf -> Some (name ^ "_log", name ^ "_lpdf")
        | Lpmf -> Some (name ^ "_log", name ^ "_lpmf")
        | Cdf -> Some (name ^ "_cdf_log", name ^ "_lcdf")
        | Ccdf -> Some (name ^ "_ccdf_log", name ^ "_lccdf")
        | Rng | UnaryVectorized -> None ) )
  |> List.map ~f:(fun (x, y) ->
         ( x
         , { replacement= y
           ; version= "2.32.0"
           ; extra_message=
               "This can be automatically changed using the canonicalize flag \
                for stanc"
           ; canonicalize_away= true } ) )
  |> String.Map.of_alist_exn

let deprecated_functions =
  let make extra_message version canonicalize_away replacement =
    {extra_message; replacement; version; canonicalize_away} in
  let ode =
    make
      "\n\
       The new interface is slightly different, see: \
       https://mc-stan.org/users/documentation/case-studies/convert_odes.html"
      "3.0" false in
  let std =
    make
      "This can be automatically changed using the canonicalize flag for stanc"
      "2.32.0" true in
  String.Map.of_alist_exn
    [ ("multiply_log", std "lmultiply")
    ; ("binomial_coefficient_log", std "lchoose")
    ; ("cov_exp_quad", std "gp_exp_quad_cov") (* ode integrators *)
    ; ("integrate_ode_rk45", ode "ode_rk45"); ("integrate_ode", ode "ode_rk45")
    ; ("integrate_ode_bdf", ode "ode_bdf")
    ; ("integrate_ode_adams", ode "ode_adams") ]

(** This function is responsible for typechecking varadic function
      calls. It needs to live in the Library since this is usually
      bespoke per-function. *)
let check_variadic_fn id ~is_cond_dist loc current_block tenv tes =
  if is_reduce_sum_fn id.Ast.name then
    Variadic_typechecking.check_reduce_sum ~is_cond_dist loc current_block tenv
      id tes
  else if is_variadic_ode_fn id.name then
    Variadic_typechecking.check_variadic_ode ~is_cond_dist loc current_block
      tenv id tes
  else if is_variadic_dae_fn id.name then
    Variadic_typechecking.check_variadic_dae ~is_cond_dist loc current_block
      tenv id tes
  else
    Common.FatalError.fatal_error_msg
      [%message
        "Invalid variadic function for Stan Math backend" (id.name : string)]

let variadic_function_returntype =
  Variadic_typechecking.variadic_function_returntype
