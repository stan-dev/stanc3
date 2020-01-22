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

(* Warning for all uniform distributions with a parameter *)
let uniform_dist_warning (dist_info : dist_info) : string option =
  match dist_info with
  | {args=(Param (pname, bounds), _)::(arg1,_)::(arg2,_)::_; _} ->
    let warning =
      Some ("Warning: At " ^ Location_span.to_string dist_info.loc ^ ", your Stan program has a uniform distribution on variable " ^ pname ^ ". The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).\n")
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


(* Warning particular to gamma and inv_gamma, when A=B<1 *)
let gamma_arg_dist_warning (dist_info : dist_info) : string option =
  match dist_info with
  | {args= [ _; (Number (a, _), _); (Number (b, _), _) ]; _} ->
    if a = b && a < 1. then
      Some ("Warning: At " ^ Location_span.to_string dist_info.loc ^ " your Stan program has a gamma or inverse-gamma model with parameters that are equal to each other and set to values less than 1. This is mathematically acceptable and can make sense in some problems, but typically we see this model used as an attempt to assign a noninformative prior distribution. In fact, priors such as inverse-gamma(.001,.001) can be very strong, as explained by Gelman (2006). Instead we recommend something like a normal(0,1) or student_t(4,0,1), with parameter constrained to be positive.\n")
    else None
  | _ -> None

(* Warning when the dist's parameter should be bounded >0 *)
let positive_dist_warning (dist_info : dist_info) : string option =
  match dist_info with
  | {args=(Param (pname, {lower; _}), _)::_; _} ->
    let warn =
      Some ("Warning: Parameter " ^ pname ^ " is given a positive distribution at " ^ Location_span.to_string dist_info.loc ^ " but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.\n")
    in
    (match lower with
     | `None -> warn
     | `Lit l when l < 0. -> warn
     | _ -> None)
  | _ -> None

let positive_named_arg_dist_warning (argn : int)
    (arg_name : string) ({args; name; loc} : dist_info) : string option =
  let v = match (List.nth args argn) with
    | Some v -> v
    | None ->
        (raise (Failure ("Distribution " ^ name
                         ^ " at " ^ Location_span.to_string loc
                         ^ " expects more arguments." )))
  in
  match v with
  | (Param (pname, {lower; _}), _) ->
    let warn =
      Some ("Warning: Parameter " ^ pname ^ " is used as " ^ arg_name ^ " at " ^ Location_span.to_string loc ^ ", but is not constrained to be positive.\n")
    in
    (match lower with
     | `None -> warn
     | `Lit l when l < 0. -> warn
     | _ -> None)
  | (Number (num, num_str), _) ->
    if num <= 0. then
      Some ("Warning: " ^ arg_name ^ " at " ^ Location_span.to_string loc ^ " is " ^ num_str ^", but " ^ arg_name ^ " should be non-negative.\n")
    else None
  | _ -> None

(* Warning when the dist's parameter should be bounded >0 *)
let unit_dist_warning (dist_info : dist_info) : string option =
  match dist_info with
  | {args=(Param (pname, {lower; upper}), _)::_; _} ->
    let warn =
      Some ("Warning: Parameter " ^ pname ^ " is given a distribution on [0,1] at " ^ Location_span.to_string dist_info.loc ^ " but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.\n")
    in
    (match (lower, upper) with
     | (`None, _) -> warn
     | (_, `None) -> warn
     | (`Lit l, _) when l < 0. -> warn
     | (_, `Lit l) when l > 1. -> warn
     | _ -> None)
  | _ -> None


let unit_named_arg_dist_warning (argn : int)
    (arg_name : string) ({args; name; loc} : dist_info) : string option =
  let v =
    Option.value
      ~default:(raise (Failure ("Distribution " ^ name ^
                                " at " ^ Location_span.to_string loc
                                ^ " expects more arguments." )))
      (List.nth args argn)
  in
  match v with
  | (Param (pname, {lower; upper}), _) ->
    let warn =
      Some ("Warning: Parameter " ^ pname ^ " is used as " ^ arg_name ^ " at " ^ Location_span.to_string loc ^ ", but is not constrained to be on [0,1].\n")
    in
    (match (lower, upper) with
     | (`None, _) -> warn
     | (_, `None) -> warn
     | (`Lit l, _) when l < 0. -> warn
     | (_, `Lit l) when l > 1. -> warn
     | _ -> None)
  | (Number (num, num_str), _) ->
    if num < 0. || num > 1. then
      Some ("Warning: " ^ arg_name ^ " at " ^ Location_span.to_string loc ^ " is " ^ num_str ^", but " ^ arg_name ^ " should be in [0,1].\n")
    else None
  | _ -> None

(* Generate the warnings that are relevant to a given distribution *)
let distribution_warning (dist_info : dist_info) : string List.t =
  let apply_warnings = List.filter_map ~f:(fun f -> f dist_info) in
  let scale_name = "a scale parameter" in
  let inv_scale_name = "an inverse scale parameter" in
  let shape_name = "a shape parameter" in
  let dof_name = "degrees of freedom" in
  let dist_name =
    match (String.chop_suffix ~suffix:"_propto_log" dist_info.name) with
    | Some name -> name
    | None -> (raise (Failure
                        ("Compiler issue: Unrecognized distribution collected: \""
                         ^ dist_info.name)))
  in
  (* Information mostly from:
     https://mc-stan.org/docs/2_21/functions-reference/unbounded-continuous-distributions.html
  *)
  match dist_name with
  (* Unbounded Continuous Distributions *)
  | "normal" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ]
  | "normal_id_glm" -> apply_warnings [
      positive_named_arg_dist_warning 4 scale_name
    ]
  | "exp_mod_normal" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ; positive_named_arg_dist_warning 3 shape_name
    ]
  | "skew_normal" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ]
  | "student_t" -> apply_warnings [
      positive_named_arg_dist_warning 1 dof_name
    ; positive_named_arg_dist_warning 3 scale_name
    ]
  | "cauchy_propto_log" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ]
  | "double_exponential" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ]
  | "logistic" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ]
  | "gumbel" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ]
  (* Positive Continuous Distributions *)
  | "lognormal" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 2 scale_name
    ]
  | "chi_square" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 dof_name
    ]
  | "inv_chi_square" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 dof_name
    ]
  | "scaled_inv_chi_square" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 dof_name
    ; positive_named_arg_dist_warning 2 scale_name
    ]
  | "exponential" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 scale_name
    ]
  | "gamma" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 shape_name
    ; positive_named_arg_dist_warning 2 inv_scale_name
    ; gamma_arg_dist_warning
    ]
  | "inv_gamma" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 shape_name
    ; positive_named_arg_dist_warning 2 scale_name
    ; gamma_arg_dist_warning
    ]
  | "weibull" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 shape_name
    ; positive_named_arg_dist_warning 2 scale_name
    ]
  | "frechet" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 shape_name
    ; positive_named_arg_dist_warning 2 scale_name
    ]
  (* Non-negative Continuous Distributions *)
  (* No real distinction needed here between positive and non-negative lower
     bounds *)
  | "rayleigh" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 scale_name
    ]
  | "weiner" -> apply_warnings [
      (* Could do more here, since variate should be > arg 2 *)
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 "a boundary separation parameter"
    ; positive_named_arg_dist_warning 2 "a non-decision time parameter"
    ; unit_named_arg_dist_warning 3 "an a-priori bias parameter"
    ]
  (* Positive Lower-Bounded Probabilities *)
  (* Currently treating these as if they're positive bounded,
     could easily do better *)
  | "pareto" -> apply_warnings [
      positive_dist_warning
    ; positive_named_arg_dist_warning 1 "a positive minimum parameter"
    ; positive_named_arg_dist_warning 2 shape_name
    ]
  | "pareto_type_2" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ; positive_named_arg_dist_warning 3 shape_name
    ]
  (* Continuous Distributions on [0,1] *)
  | "beta" -> apply_warnings [
      unit_dist_warning
    ; positive_named_arg_dist_warning 1 "a count parameter"
    ; positive_named_arg_dist_warning 2 "a count parameter"
    ]
  | "beta_proportion" -> apply_warnings [
      unit_dist_warning
    (* should be exclusive bounds on arg 1*)
    ; unit_named_arg_dist_warning 1 "a unit mean parameter"
    ; positive_named_arg_dist_warning 2 "a precision parameter"
    ]
  (* Circular Distributions *)
  | "von_mises" -> apply_warnings [
      positive_named_arg_dist_warning 2 scale_name
    ]
  (* Bounded Continuous Distributions *)
  | "uniform" -> apply_warnings [
      (* Could also check b > c *)
      uniform_dist_warning
    ]
      (* Simplex Distributions *)
  | "dirichlet" -> apply_warnings [
      unit_dist_warning
    ; positive_named_arg_dist_warning 1 "a count parameter"
    ]
  (* TODO: Multivariate distributions in sections
      Distributions over Unbounded Vectors
      Correlation Matrix Distributions
      Covariance Matrix Distributions
  *)
  | _ -> []


(* Generate the distribution warnings for a program *)
let list_distribution_warnings (distributions_list : dist_info Set.Poly.t) : string Set.Poly.t =
  union_map
    ~f:(fun dist_info ->
        Set.Poly.of_list (distribution_warning dist_info))
    distributions_list
