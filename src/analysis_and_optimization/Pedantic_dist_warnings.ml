open Core_kernel
open Middle
open Mir_utils

(* Useful information about an expression. Opaque means we don't know anything. *)
type compiletime_val
  = Opaque
  | Number of (float * string)
  | Param of (string * bound_values)
  | Data of string

(* Info about a distribution occurrences that's useful for checking that
   distribution properties are met
*)
type dist_info =
  { name : string
  ; loc : Location_span.t
  ; args : (compiletime_val * Expr.Typed.Meta.t) List.t
  }

let uniform_dist_message (pname : string) : string =
  Printf.sprintf
    "Parameter %s is given a uniform distribution. The uniform distribution is \
     not recommended, for two reasons: (a) Except when there are logical or \
     physical constraints, it is very unusual for you to be sure that a \
     parameter will fall inside a specified range, and (b) The infinite \
     gradient induced by a uniform density can cause difficulties for Stan's \
     sampling algorithm. As a consequence, we recommend soft constraints rather \
     than hard constraints; for example, instead of giving an elasticity \
     parameter a uniform(0,1) distribution, try normal(0.5,0.5)."
    pname

(* Warning for all uniform distributions with a parameter *)
let uniform_dist_warning (dist_info : dist_info) : (Location_span.t * string) option =
  match dist_info with
  | {args=(Param (pname, bounds), _)::(arg1,_)::(arg2,_)::_; _} ->
    let warning =
      Some (dist_info.loc, uniform_dist_message pname)
    in
    (match (arg1, arg2, bounds) with
     | (_, _, {upper = `None; _})
     | (_, _, {lower = `None; _}) ->
       (* the variate is unbounded *)
       warning
     | (Number (uni, _), _, {lower = `Lit bound; _})
     | (_, Number (uni, _), {upper = `Lit bound; _}) ->
       (* the variate is bounded differently than the uniform dist *)
       if uni = bound then
         None
       else
         warning
     | _ -> None)
  | _ -> None

let gamma_arg_dist_message : string =
  "There is a gamma or inverse-gamma distribution with parameters that are \
   equal to each other and set to values less than 1. This is mathematically \
   acceptable and can make sense in some problems, but typically we see this \
   model used as an attempt to assign a noninformative prior distribution. In \
   fact, priors such as inverse-gamma(.001,.001) can be very strong, as \
   explained by Gelman (2006). Instead we recommend something like a \
   normal(0,1) or student_t(4,0,1), with parameter constrained to be \
   positive."

(* Warning particular to gamma and inv_gamma, when A=B<1 *)
let gamma_arg_dist_warning (dist_info : dist_info) : (Location_span.t * string) option =
  match dist_info with
  | {args= [ _; (Number (a, _), meta); (Number (b, _), _) ]; _} ->
    if a = b && a < 1. then
      Some (meta.loc, gamma_arg_dist_message)
    else None
  | _ -> None

type range =
  { name : string
  ; lower : (float * bool) option
  ; upper : (float * bool) option
  }

let unit_range =
  { name = "[0,1]"
  ; lower = Some (0., true)
  ; upper = Some (1., true)
  }

let positive_range =
  { name = "positive"
  ; lower = Some (0., false)
  ; upper = None
  }

let bounds_out_of_range (range : range) (bounds : bound_values) =
  match (bounds.lower, bounds.upper, range.lower, range.upper) with
   | (`None, _, Some _, _) -> true
   | (_, `None, _, Some _) -> true
   | (`Lit l, _, Some (l', _), _) when l < l' -> true
   | (_, `Lit u, _, Some (u', _)) when u > u' -> true
   | _ -> false

let value_out_of_range (range : range) (v : float) =
  let lower_bad = match range.lower with
    | Some (l, true) -> v < l
    | Some (l, false) -> v <= l
    | None -> false
  in
  let upper_bad = match range.upper with
    | Some (u, true) -> v > u
    | Some (u, false) -> v >= u
    | None -> false
  in lower_bad || upper_bad

let arg_range_bounds_message (dist_name : string) (param_name : string)
    (arg_name : string) (argn : int) (range_name : string) : string =
  Printf.sprintf
    "A %s distribution has parameter %s as %s (argument %d), but %s is not \
     constrained to be %s."
    dist_name param_name arg_name argn param_name range_name

let arg_range_literal_message (dist_name : string) (num_str : string)
    (arg_name : string) (argn : int) (range_name : string) : string =
  Printf.sprintf
    "A %s distribution has value %s as %s (argument %d), but %s should be %s."
    dist_name num_str arg_name argn arg_name range_name

let arg_range_warning (range : range) (argn : int) (arg_name : string)
    ({args; name; loc} : dist_info) : (Location_span.t * string) option =
  let v = match (List.nth args argn) with
    | Some v -> v
    | None ->
      (raise (Failure ("Distribution " ^ name
                       ^ " at " ^ Location_span.to_string loc
                       ^ " expects more arguments." )))
  in
  match v with
  | (Param (pname, bounds), meta) ->
    if bounds_out_of_range range bounds then
      Some ( meta.loc
           , arg_range_bounds_message name pname arg_name argn range.name)
    else None
  | (Number (num, num_str), meta) ->
    if value_out_of_range range num then
      Some ( meta.loc
           , arg_range_literal_message name num_str arg_name argn range.name)
    else None
  | _ -> None

let variate_range_bounds_message (dist_name : string) (param_name : string)
    (range_name : string) : string =
  Printf.sprintf
    "Parameter %s is given a %s distribution, which has %s range, but was \
     declared with no constraints or incompatible constraints. Either \
     change the distribution or change the constraints."
    param_name dist_name range_name

(* Warning when the dist's parameter should be bounded >0 *)
let variate_range_warning (range : range) (dist_info : dist_info)
  : (Location_span.t * string) option =
  match dist_info with
  | {args=(Param (pname, bounds), meta)::_; _} ->
    if bounds_out_of_range range bounds then
      Some ( meta.loc
           , variate_range_bounds_message dist_info.name pname range.name)
    else None
  | _ -> None

(* Generate the warnings that are relevant to a given distribution *)
let distribution_warning (dist_info : dist_info)
  : (Location_span.t * string) List.t =
  let apply_warnings = List.filter_map ~f:(fun f -> f dist_info) in
  let scale_name = "a scale parameter" in
  let inv_scale_name = "an inverse scale parameter" in
  let shape_name = "a shape parameter" in
  let dof_name = "degrees of freedom" in
  (* Information mostly from:
     https://mc-stan.org/docs/2_21/functions-reference/unbounded-continuous-distributions.html
  *)
  match dist_info.name with
  (* Unbounded Continuous Distributions *)
  | "normal" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ]
  | "normal_id_glm" -> apply_warnings [
      arg_range_warning positive_range 4 scale_name
    ]
  | "exp_mod_normal" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ; arg_range_warning positive_range 3 shape_name
    ]
  | "skew_normal" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ]
  | "student_t" -> apply_warnings [
      arg_range_warning positive_range 1 dof_name
    ; arg_range_warning positive_range 3 scale_name
    ]
  | "cauchy" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ]
  | "double_exponential" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ]
  | "logistic" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ]
  | "gumbel" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ]
  (* Positive Continuous Distributions *)
  | "lognormal" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 2 scale_name
    ]
  | "chi_square" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 dof_name
    ]
  | "inv_chi_square" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 dof_name
    ]
  | "scaled_inv_chi_square" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 dof_name
    ; arg_range_warning positive_range 2 scale_name
    ]
  | "exponential" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 scale_name
    ]
  | "gamma" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 shape_name
    ; arg_range_warning positive_range 2 inv_scale_name
    ; gamma_arg_dist_warning
    ]
  | "inv_gamma" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 shape_name
    ; arg_range_warning positive_range 2 scale_name
    ; gamma_arg_dist_warning
    ]
  | "weibull" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 shape_name
    ; arg_range_warning positive_range 2 scale_name
    ]
  | "frechet" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 shape_name
    ; arg_range_warning positive_range 2 scale_name
    ]
  (* Non-negative Continuous Distributions *)
  (* No real distinction needed here between positive and non-negative lower
     bounds *)
  | "rayleigh" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 scale_name
    ]
  | "wiener" -> apply_warnings [
      (* Could do more here, since variate should be > arg 2 *)
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 "a boundary separation parameter"
    ; arg_range_warning positive_range 2 "a non-decision time parameter"
    ; arg_range_warning unit_range 3 "an a-priori bias parameter"
    ]
  (* Positive Lower-Bounded Probabilities *)
  (* Currently treating these as if they're positive bounded,
     could easily do better *)
  | "pareto" -> apply_warnings [
      variate_range_warning positive_range
    ; arg_range_warning positive_range 1 "a positive minimum parameter"
    ; arg_range_warning positive_range 2 shape_name
    ]
  | "pareto_type_2" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ; arg_range_warning positive_range 3 shape_name
    ]
  (* Continuous Distributions on [0,1] *)
  | "beta" -> apply_warnings [
      variate_range_warning unit_range
    ; arg_range_warning positive_range 1 "a count parameter"
    ; arg_range_warning positive_range 2 "a count parameter"
    ]
  | "beta_proportion" -> apply_warnings [
      variate_range_warning unit_range
    (* should be exclusive bounds on arg 1*)
    ; arg_range_warning unit_range 1 "a unit mean parameter"
    ; arg_range_warning positive_range 2 "a precision parameter"
    ]
  (* Circular Distributions *)
  | "von_mises" -> apply_warnings [
      arg_range_warning positive_range 2 scale_name
    ]
  (* Bounded Continuous Distributions *)
  | "uniform" -> apply_warnings [
      (* Could also check b > c *)
      uniform_dist_warning
    ]
      (* Simplex Distributions *)
  | "dirichlet" -> apply_warnings [
      variate_range_warning unit_range
    ; arg_range_warning positive_range 1 "a count parameter"
    ]
  (* TODO: Multivariate distributions in sections
      Distributions over Unbounded Vectors
      Correlation Matrix Distributions
      Covariance Matrix Distributions
  *)
  | _ -> []

(* Generate the distribution warnings for a program *)
let list_distribution_warnings (distributions_list : dist_info Set.Poly.t)
  : (Location_span.t * string) Set.Poly.t =
  union_map
    ~f:(fun dist_info ->
        Set.Poly.of_list (distribution_warning dist_info))
    distributions_list
