open Core_kernel
open Middle
open Mir_utils

(*********************
   Types and utilities
 ********************)

(** Useful information about an expression. [Opaque] means we don't know anything. *)
type compiletime_val =
  | Opaque
  | Number of (float * string)
  | Param of (string * Expr.Typed.t Transformation.t)
  | Data of string

(** Info about a distribution occurrences that's useful for checking that
   distribution properties are met
*)
type dist_info =
  { name: string
  ; loc: Location_span.t
  ; args: (compiletime_val * Expr.Typed.Meta.t) List.t }

(** Value constraint as a range. The bools are true if the bound is inclusive *)
type range = {lower: (float * bool) option; upper: (float * bool) option}

(** Value constraint for an argument *)
type var_constraint =
  | Range of range
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance

(** Constraint paired with a name for user messages *)
type var_constraint_named = {name: string; constr: var_constraint}

let unit_range =
  { name= "[0,1]"
  ; constr= Range {lower= Some (0., true); upper= Some (1., true)} }

let exclusive_unit_range =
  { name= "(0,1)"
  ; constr= Range {lower= Some (0., false); upper= Some (1., false)} }

let positive_range =
  { name= "strictly positive"
  ; constr= Range {lower= Some (0., false); upper= None} }

let nonnegative_range =
  {name= "non-negative"; constr= Range {lower= Some (0., true); upper= None}}

let simplex = {name= "simplex"; constr= Simplex}
let ordered = {name= "ordered"; constr= Ordered}
let correlation = {name= "correlation"; constr= Correlation}

let cholesky_correlation =
  {name= "Cholesky factor of correlation"; constr= CholeskyCorr}

let covariance = {name= "covariance"; constr= Covariance}

let cholesky_covariance =
  {name= "Cholesky factor of covariance"; constr= CholeskyCov}

(** Check for inconsistency between a distribution argument's value range and the
   declared bounds of a variable *)
let bounds_out_of_range (range : range) (bounds : bound_values) : bool =
  match (bounds.lower, bounds.upper, range.lower, range.upper) with
  | `None, _, Some _, _ -> true
  | _, `None, _, Some _ -> true
  | `Lit l, _, Some (l', _), _ when l < l' -> true
  | _, `Lit u, _, Some (u', _) when u > u' -> true
  | _ -> false

(** Check for inconsistency between a distribution argument's constraint and the
   constraint transformation of a variable *)
let transform_mismatch_constraint (constr : var_constraint)
    (trans : Expr.Typed.t Transformation.t) : bool =
  match constr with
  | Range range -> bounds_out_of_range range (trans_bounds_values trans)
  | Ordered -> trans <> Transformation.Ordered
  | PositiveOrdered -> trans <> PositiveOrdered
  | Simplex -> trans <> Simplex
  | UnitVector -> trans <> UnitVector
  | CholeskyCorr -> trans <> CholeskyCorr
  | CholeskyCov -> trans <> CholeskyCov && trans <> CholeskyCorr
  | Correlation -> trans <> Correlation
  | Covariance -> trans <> Covariance && trans <> Correlation

(** Check for inconsistency between a distribution argument's range and
   a literal value *)
let value_out_of_range (range : range) (v : float) =
  let lower_bad =
    match range.lower with
    | Some (l, true) -> v < l
    | Some (l, false) -> v <= l
    | None -> false
  in
  let upper_bad =
    match range.upper with
    | Some (u, true) -> v > u
    | Some (u, false) -> v >= u
    | None -> false
  in
  lower_bad || upper_bad

(** Check for inconsistency between a distribution argument's constraint and
   a literal value *)
let value_mismatch_constraint (constr : var_constraint) (v : float) =
  match constr with
  | Range range -> value_out_of_range range v
  (* We don't know how to check if a value falls into a constraint other than
     Range, unless we want to inspect e.g. matrix literals. *)
  | _ -> false

(*********************
   Argument constraint mismatch warnings
 ********************)

type arg_info = Arg of (int * string) | Variate

let arg_number (arg : arg_info) =
  match arg with Arg (n, _) -> n | Variate -> 0

let constr_mismatch_message (dist_name : string) (param_name : string)
    (arg : arg_info) (constr_name : string) : string =
  match arg with
  | Arg (argn, arg_name) ->
      Printf.sprintf
        "A %s distribution is given parameter %s as %s (argument %d), but %s \
         was not constrained to be %s."
        dist_name param_name arg_name argn param_name constr_name
  | Variate ->
      (* Possibly: Either change the distribution or change the constraints. *)
      Printf.sprintf
        "Parameter %s is given a %s distribution, which has %s support, but \
         %s was not constrained to be %s."
        param_name dist_name constr_name param_name constr_name

let constr_literal_mismatch_message (dist_name : string) (num_str : string)
    (arg : arg_info) (constr_name : string) : string =
  match arg with
  | Arg (argn, arg_name) ->
      Printf.sprintf
        "A %s distribution is given value %s as %s (argument %d), but %s is \
         not %s."
        dist_name num_str arg_name argn arg_name constr_name
  | Variate ->
      (* Possibly: Either change the distribution or change the constraints. *)
      Printf.sprintf
        "Value %s is given a %s distribution, which has %s support, but %s is \
         not %s."
        num_str dist_name constr_name num_str constr_name

(** Return a warning if the argn-th argument doesn't match its constraints *)
let constr_mismatch_warning (constr : var_constraint_named) (arg : arg_info)
    ({args; name; loc} : dist_info) : (Location_span.t * string) option =
  let v =
    match List.nth args (arg_number arg) with
    | Some v -> v
    | None ->
        let arg_fail_msg =
          Printf.sprintf "Distribution %s at %s expects more arguments." name
            (Location_span.to_string loc)
        in
        raise (Failure arg_fail_msg)
  in
  match v with
  | Param (pname, trans), meta ->
      if transform_mismatch_constraint constr.constr trans then
        Some (meta.loc, constr_mismatch_message name pname arg constr.name)
      else None
  | Number (num, num_str), meta ->
      if value_mismatch_constraint constr.constr num then
        Some
          ( meta.loc
          , constr_literal_mismatch_message name num_str arg constr.name )
      else None
  | _ -> None

(*********************
   Distribution-specific warnings
 ********************)

let uniform_dist_message (pname : string) : string =
  Printf.sprintf
    "Parameter %s is given a uniform distribution. The uniform distribution \
     is not recommended, for two reasons: (a) Except when there are logical \
     or physical constraints, it is very unusual for you to be sure that a \
     parameter will fall inside a specified range, and (b) The infinite \
     gradient induced by a uniform density can cause difficulties for Stan's \
     sampling algorithm. As a consequence, we recommend soft constraints \
     rather than hard constraints; for example, instead of giving an \
     elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5)."
    pname

(** Warning for all uniform distributions with a parameter *)
let uniform_dist_warning (dist_info : dist_info) :
    (Location_span.t * string) option =
  match dist_info with
  | {args= (Param (pname, trans), _) :: (arg1, _) :: (arg2, _) :: _; _} -> (
      let warning = Some (dist_info.loc, uniform_dist_message pname) in
      match (arg1, arg2, trans_bounds_values trans) with
      | _, _, {upper= `None; _} | _, _, {lower= `None; _} ->
          (* the variate is unbounded *)
          warning
      | Number (uni, _), _, {lower= `Lit bound; _}
       |_, Number (uni, _), {upper= `Lit bound; _} ->
          (* the variate is bounded differently than the uniform dist *)
          if uni = bound then None else warning
      | _ -> None )
  | _ -> None

let lkj_corr_message : string =
  "It is suggested to reparameterize your model to replace lkj_corr with \
   lkj_corr_cholesky, the Cholesky factor variant. lkj_corr tends to run \
   slower, consume more memory, and has higher risk of numerical errors."

(** Warn about all non-Cholesky lkj_corr distributions *)
let lkj_corr_dist_warning (dist_info : dist_info) :
    (Location_span.t * string) option =
  Some (dist_info.loc, lkj_corr_message)

let gamma_arg_dist_message : string =
  "There is a gamma or inverse-gamma distribution with parameters that are \
   equal to each other and set to values less than 1. This is mathematically \
   acceptable and can make sense in some problems, but typically we see this \
   model used as an attempt to assign a noninformative prior distribution. In \
   fact, priors such as inverse-gamma(.001,.001) can be very strong, as \
   explained by Gelman (2006). Instead we recommend something like a \
   normal(0,1) or student_t(4,0,1), with parameter constrained to be positive."

(** Warning particular to gamma and inv_gamma, when A=B<1 *)
let gamma_arg_dist_warning (dist_info : dist_info) :
    (Location_span.t * string) option =
  match dist_info with
  | {args= [_; (Number (a, _), meta); (Number (b, _), _)]; _} ->
      if a = b && a < 1. then Some (meta.loc, gamma_arg_dist_message) else None
  | _ -> None

(*********************
   Distribution properties table
 ********************)

(** Generate all of the warnings that are relevant to a given distribution *)
let distribution_warning (dist_info : dist_info) :
    (Location_span.t * string) List.t =
  let scale_name = "a scale parameter" in
  let scale_mat_name = "a scale matrix" in
  let inv_scale_name = "an inverse scale parameter" in
  let shape_name = "a shape parameter" in
  let dof_name = "degrees of freedom" in
  let cov_name = "a covariance matrix" in
  (* Information mostly from:
     https://mc-stan.org/docs/2_21/functions-reference/unbounded-continuous-distributions.html
  *)
  let warning_fns =
    match dist_info.name with
    (* Binary Distributions *)
    | "bernoulli" ->
        [ (* Note: variate binary *)
          constr_mismatch_warning unit_range (Arg (1, "chance of success")) ]
    | "bernoulli_logit" -> [ (* Note: variate binary *) ]
    | "bernoulli_logit_glm" -> [ (* Note: variate binary *) ]
    (* Bounded Discrete Distributions *)
    | "binomial" ->
        [ (* Note: variate nonnegative int *)
          (* Note: args 1 nonnegative int *)
          constr_mismatch_warning unit_range (Arg (2, "chance of success")) ]
    | "binomial_logit" ->
        [ (* Note: variate nonnegative int *)
          (* Note: args 1 nonnegative int *) ]
    | "beta_binomial" ->
        [ (* Note: variate nonnegative int *)
          (* Note: args 1 nonnegative int *)
          constr_mismatch_warning positive_range
            (Arg (2, "a prior success count"))
        ; constr_mismatch_warning positive_range
            (Arg (3, "a prior failure count")) ]
    | "hypergeometric" ->
        [ (* Note: variate nonnegative int *)
          (* Note: args 1,2,3 nonnegative int *) ]
    | "categorical" ->
        [ (* Note: variate positive int *)
          constr_mismatch_warning simplex
            (Arg (1, "a vector of outcome probabilities")) ]
    | "ordered_logistic" ->
        [ (* Note: variate positive int *)
          constr_mismatch_warning ordered (Arg (2, "cutpoints")) ]
    | "ordered_probit" ->
        [ (* Note: variate positive int *)
          constr_mismatch_warning ordered (Arg (2, "cutpoints")) ]
    (* Unbounded Discrete Distributions *)
    | "neg_binomial" ->
        [ (* Note: variate nonnegative int *)
          constr_mismatch_warning positive_range (Arg (1, shape_name))
        ; constr_mismatch_warning positive_range (Arg (2, inv_scale_name)) ]
    | "neg_binomial_2" ->
        [ (* Note: variate nonnegative int *)
          constr_mismatch_warning positive_range (Arg (1, shape_name))
        ; constr_mismatch_warning positive_range
            (Arg (2, "a precision parameter")) ]
    | "neg_binomial_2_log" ->
        [ (* Note: variate nonnegative int *)
          constr_mismatch_warning positive_range
            (Arg (2, "an inverse overdispersion control parameter")) ]
    | "neg_binomial_2_log_glm" ->
        [ (* Note: variate nonnegative int *)
          constr_mismatch_warning positive_range
            (Arg (4, "an inverse overdispersion control parameter")) ]
    | "poisson" ->
        [ (* Note: variate nonnegative int *)
          constr_mismatch_warning positive_range (Arg (1, "a rate parameter"))
        ]
    | "poisson_log" -> [ (* Note: variate nonnegative int *) ]
    | "poisson_log_glm" -> [ (* Note: variate nonnegative int *) ]
    (* Multivariate Discrete Distributions *)
    | "multinomial" ->
        [ (* Note: variate nonnegative int *)
          constr_mismatch_warning simplex (Arg (1, "a distribution parameter"))
        ]
    (* Unbounded Continuous Distributions *)
    | "normal" -> [constr_mismatch_warning positive_range (Arg (2, scale_name))]
    | "normal_id_glm" ->
        [constr_mismatch_warning positive_range (Arg (4, scale_name))]
    | "exp_mod_normal" ->
        [ constr_mismatch_warning positive_range (Arg (2, scale_name))
        ; constr_mismatch_warning positive_range (Arg (3, shape_name)) ]
    | "skew_normal" ->
        [constr_mismatch_warning positive_range (Arg (2, scale_name))]
    | "student_t" ->
        [ constr_mismatch_warning positive_range (Arg (1, dof_name))
        ; constr_mismatch_warning positive_range (Arg (3, scale_name)) ]
    | "cauchy" -> [constr_mismatch_warning positive_range (Arg (2, scale_name))]
    | "double_exponential" ->
        [constr_mismatch_warning positive_range (Arg (2, scale_name))]
    | "logistic" ->
        [constr_mismatch_warning positive_range (Arg (2, scale_name))]
    | "gumbel" -> [constr_mismatch_warning positive_range (Arg (2, scale_name))]
    (* Positive Continuous Distributions *)
    | "lognormal" ->
        [ constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range (Arg (2, scale_name)) ]
    | "chi_square" ->
        [ constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, dof_name)) ]
    | "inv_chi_square" ->
        [ constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, dof_name)) ]
    | "scaled_inv_chi_square" ->
        [ constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, dof_name))
        ; constr_mismatch_warning positive_range (Arg (2, scale_name)) ]
    | "exponential" ->
        [ constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, scale_name)) ]
    | "gamma" ->
        [ constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, shape_name))
        ; constr_mismatch_warning positive_range (Arg (2, inv_scale_name))
        ; gamma_arg_dist_warning ]
    | "inv_gamma" ->
        [ constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, shape_name))
        ; constr_mismatch_warning positive_range (Arg (2, scale_name))
        ; gamma_arg_dist_warning ]
    | "weibull" ->
        [ constr_mismatch_warning nonnegative_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, shape_name))
        ; constr_mismatch_warning positive_range (Arg (2, scale_name)) ]
    | "frechet" ->
        [ constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, shape_name))
        ; constr_mismatch_warning positive_range (Arg (2, scale_name)) ]
    (* Non-negative Continuous Distributions *)
    | "rayleigh" ->
        [ constr_mismatch_warning nonnegative_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, scale_name)) ]
    | "wiener" ->
        [ (* Note: Could do more here, since variate should be > arg 2 *)
          constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range
            (Arg (1, "a boundary separation parameter"))
        ; constr_mismatch_warning positive_range
            (Arg (2, "a non-decision time parameter"))
        ; constr_mismatch_warning unit_range
            (Arg (3, "an a-priori bias parameter")) ]
    (* Positive Lower-Bounded Probabilities *)
    | "pareto" ->
        [ (* Note: Variate >= arg 1 *)
          constr_mismatch_warning positive_range Variate
        ; constr_mismatch_warning positive_range
            (Arg (1, "a positive minimum parameter"))
        ; constr_mismatch_warning positive_range (Arg (2, shape_name)) ]
    | "pareto_type_2" ->
        [ (* Note: Variate >= arg 1 *)
          constr_mismatch_warning positive_range (Arg (2, scale_name))
        ; constr_mismatch_warning positive_range (Arg (3, shape_name)) ]
    (* Continuous Distributions on [0,1] *)
    | "beta" ->
        [ constr_mismatch_warning exclusive_unit_range Variate
        ; constr_mismatch_warning positive_range (Arg (1, "a count parameter"))
        ; constr_mismatch_warning positive_range (Arg (2, "a count parameter"))
        ]
    | "beta_proportion" ->
        [ constr_mismatch_warning exclusive_unit_range Variate
        ; constr_mismatch_warning exclusive_unit_range
            (Arg (1, "a unit mean parameter"))
        ; constr_mismatch_warning positive_range
            (Arg (2, "a precision parameter")) ]
    (* Circular Distributions *)
    | "von_mises" ->
        [constr_mismatch_warning positive_range (Arg (2, scale_name))]
    (* Bounded Continuous Distributions *)
    | "uniform" ->
        [ (* Could also check b > c *)
          (* Can this be generalized, by restricting a < variate < b? *)
          uniform_dist_warning ]
    (* Distributions over Unbounded Vectors *)
    | "multi_normal" -> [constr_mismatch_warning covariance (Arg (2, cov_name))]
    | "multi_normal_prec" ->
        [constr_mismatch_warning covariance (Arg (2, "a precision matrix"))]
    | "multi_normal_cholesky" ->
        [constr_mismatch_warning cholesky_covariance (Arg (2, cov_name))]
    | "multi_gp" ->
        [ (* Note: arg 2 "inverse scales" is vector of positive inverse scales*)
          constr_mismatch_warning covariance (Arg (1, "a kernel matrix")) ]
    | "multi_gp_cholesky" ->
        [ (* Note: arg 2 "inverse scales" is vector of positive inverse scales*)
          constr_mismatch_warning cholesky_covariance
            (Arg (1, "Cholesky factor of the kernel matrix")) ]
    | "multi_student_t" ->
        [ constr_mismatch_warning positive_range (Arg (1, dof_name))
        ; constr_mismatch_warning covariance (Arg (3, scale_mat_name)) ]
    | "gaussian_dlm_obs" ->
        [ constr_mismatch_warning covariance
            (Arg (3, "observation covariance matrix"))
        ; constr_mismatch_warning covariance
            (Arg (4, "system covariance matrix")) ]
    (* Simplex Distributions *)
    | "dirichlet" ->
        [ constr_mismatch_warning simplex Variate
        ; constr_mismatch_warning positive_range (Arg (1, "a count parameter"))
        ]
    (* Correlation Matrix Distributions *)
    | "lkj_corr" ->
        [ lkj_corr_dist_warning
        ; constr_mismatch_warning correlation Variate
        ; constr_mismatch_warning positive_range (Arg (1, shape_name)) ]
    | "lkj_corr_cholesky" ->
        [ constr_mismatch_warning cholesky_correlation Variate
        ; constr_mismatch_warning positive_range (Arg (1, shape_name)) ]
    (* Covariance Matrix Distributions *)
    | "wishart" ->
        [ constr_mismatch_warning covariance Variate
        ; constr_mismatch_warning positive_range (Arg (1, dof_name))
        ; constr_mismatch_warning covariance (Arg (2, scale_mat_name)) ]
    | "inv_wishart" ->
        [ constr_mismatch_warning covariance Variate
        ; constr_mismatch_warning positive_range (Arg (1, dof_name))
        ; constr_mismatch_warning covariance (Arg (2, scale_mat_name)) ]
    | _ -> []
  in
  List.filter_map ~f:(fun f -> f dist_info) warning_fns

(** Generate the distribution warnings for a program *)
let distribution_warnings (distributions_list : dist_info Set.Poly.t) :
    (Location_span.t * string) Set.Poly.t =
  union_map
    ~f:(fun dist_info -> Set.Poly.of_list (distribution_warning dist_info))
    distributions_list
