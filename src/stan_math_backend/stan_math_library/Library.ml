(** This is the {e implementation} of the Library virtual module
    for the Stan Math C++ backend
  *)

open Core_kernel
open Std_library_utils

(** Many of the required functions are exposed in the backend specific file, so we include it *)
include Stan_math_backend.Stan_math_signatures

include Stan_math_extras

let deprecated_distributions =
  List.concat_map distributions ~f:(fun (fnkinds, name, _, _) ->
      List.filter_map fnkinds ~f:(function
        | Lpdf -> Some (name ^ "_log", name ^ "_lpdf")
        | Lpmf -> Some (name ^ "_log", name ^ "_lpmf")
        | Cdf -> Some (name ^ "_cdf_log", name ^ "_lcdf")
        | Ccdf -> Some (name ^ "_ccdf_log", name ^ "_lccdf")
        | Rng | Log | UnaryVectorized _ -> None ) )
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
    ; ("integrate_ode_adams", ode "ode_adams")
    ; ("if_else", std "the conditional operator (x ? y : z)") ]

(** This function is responsible for typechecking varadic function
      calls. It needs to live in the Library since this is usually
      bespoke per-function. *)
let check_special_fn = check_reduce_sum

let is_special_function_name = is_reduce_sum_fn

let special_function_returntype name =
  if is_reduce_sum_fn name then Some (Middle.UnsizedType.ReturnType UReal)
  else None
