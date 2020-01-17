open Core_kernel
open Middle
open Middle.Program
open Middle.Expr
open Dependence_analysis
open Dataflow_types
open Dataflow_utils
open Factor_graph
open Mir_utils

type dist_info =
  { name : string
  ; loc : Location_span.t
  ; args : Expr.Typed.t List.t
  ; param_opt : (string * bound_values) option
  }

let param_info
    (params : (string * Expr.Typed.t Program.transformation) Set.Poly.t)
    (expr : Expr.Typed.t) : (string * bound_values) option =
  match expr with
  | ({pattern= Var pname; _}) ->
    Option.map
      ~f:(fun (name, trans) -> (name, trans_bounds_values trans))
      (Set.Poly.find params ~f:(fun (name, _) -> name = pname))
  | _ -> None

let list_distributions (mir : Program.Typed.t) : dist_info Set.Poly.t =
  let collect_distribution_expr s (expr : Expr.Typed.Meta.t Expr.Fixed.t) =
    match expr.pattern with
    | Expr.Fixed.Pattern.FunApp
        (StanLib, fname, args) ->
      if is_dist fname then
        let lhs = match args with
          | (lhs :: _) -> lhs
          | _ -> raise (Failure ("Found distribution " ^ fname ^ " which has no arguments"))
        in
        let params = parameter_set mir in
        let param_info_opt = param_info params lhs in
        Set.Poly.add s
          { name = fname
          ; loc = expr.meta.loc
          ; args = args
          ; param_opt = param_info_opt
          }
      else s
    | _ -> s
  in
  fold_stmts
    ~init:Set.Poly.empty
    ~take_stmt:(fun s _ -> s)
    ~take_expr:(fun s e -> collect_distribution_expr s e)
    (List.concat [mir.log_prob; List.map ~f:(fun f -> f.fdbody) mir.functions_block])

let uniform_dist_warning (dist_info : dist_info) : string option =
  match dist_info with
   | {param_opt= Some (pname, _); _} ->
     Some ("Warning: At " ^ Location_span.to_string dist_info.loc ^ ", your Stan program has a uniform distribution on variable " ^ pname ^ ". The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).\n")
   | _ -> None

let gamma_arg_dist_warning (dist_info : dist_info) : string option =
  match dist_info with
  | {args= [ _; a_expr; b_expr ]; _} ->
    (match (num_expr_value a_expr, num_expr_value b_expr) with
     | (Some a, Some b) -> if a = b && a < 1. then
         Some ("Warning: At " ^ Location_span.to_string dist_info.loc ^ " your Stan program has a gamma or inverse-gamma model with parameters that are equal to each other and set to values less than 1. This is mathematically acceptable and can make sense in some problems, but typically we see this model used as an attempt to assign a noninformative prior distribution. In fact, priors such as inverse-gamma(.001,.001) can be very strong, as explained by Gelman (2006). Instead we recommend something like a normal(0,1) or student_t(4,0,1), with parameter constrained to be positive.\n")
       else None
     | _ -> None)
  | _ -> None

let positive_dist_warning (dist_info : dist_info) : string option =
  match dist_info with
  | {param_opt= Some (pname, {lower; _}); _} ->
    let warn =
      Some ("Warning: Parameter " ^ pname ^ " is given a constrained distribution at " ^ Location_span.to_string dist_info.loc ^ " but was declared with no constraints or incompatible constraints. Either change the distribution or change the constraints.\n")
    in
    (match lower with
     | `None -> warn
     | `Lit l when l < 0. -> warn
     | _ -> None)
  | _ -> None

let distribution_warning
    (dist_info : dist_info) : string List.t =
  let apply_warnings = List.filter_map ~f:(fun f -> f dist_info) in
  match dist_info.name with
  | "uniform_propto_log" -> apply_warnings [
      uniform_dist_warning
    ]
  | "gamma_propto_log" -> apply_warnings [
      positive_dist_warning
    ; gamma_arg_dist_warning
    ]
  | "inv_gamma_propto_log" -> apply_warnings [
      gamma_arg_dist_warning
    ]
  | "lognormal_propto_log" -> apply_warnings [
      positive_dist_warning
    ]
  | _ -> []

let list_distribution_warnings (mir : Program.Typed.t) : string Set.Poly.t =
  union_map
    ~f:(fun dist_info ->
        Set.Poly.of_list (distribution_warning dist_info))
    (list_distributions mir)

let list_unused_params (mir : Program.Typed.t) : string Set.Poly.t =
  let params = parameter_names_set mir in
  let unused_params_expr (expr : Expr.Typed.t) (p : string Set.Poly.t) =
    match expr.pattern with
    | Expr.Fixed.Pattern.Var s -> Set.Poly.remove p s
    | _ -> p
  in
  fold_stmts
    ~take_stmt:(fun p _ -> p)
    ~take_expr:(fun p e -> unused_params_expr e p)
    ~init:params
    (List.concat [mir.log_prob; List.map ~f:(fun f -> f.fdbody) mir.functions_block])

let list_sigma_unbounded (mir : Program.Typed.t) :
  string Set.Poly.t =
  let not_lower_zero (e : Expr.Typed.t transformation) = match e with
    | Lower {pattern= Fixed.Pattern.Lit (Int, i); _} ->
      int_of_string i <> 0
    | Lower {pattern= Fixed.Pattern.Lit (Real, i); _} ->
      float_of_string i <> 0.
    | Lower _ -> false
    | LowerUpper ({pattern= Fixed.Pattern.Lit (Int, i); _}, _) ->
      int_of_string i <> 0
    | LowerUpper ({pattern= Fixed.Pattern.Lit (Real, i); _}, _) ->
      float_of_string i <> 0.
    | LowerUpper (_, _) -> false
    | _ -> true
  in
  Set.Poly.map ~f:fst
    (Set.Poly.filter
       ~f:(fun (name, trans) ->
           String.is_prefix ~prefix:"sigma" name
           && not_lower_zero trans)
       (parameter_set mir))

let list_hard_constrained (mir : Program.Typed.t) :
  string Set.Poly.t =
  let constrained (e : Expr.Typed.t transformation) = match e with
    | LowerUpper (lower, upper) ->
      (match (num_expr_value lower, num_expr_value upper) with
       | (Some 0., Some 1.) | (Some -1., Some 1.) -> false
       | _ -> true
      )
    | _ -> false
  in
  Set.Poly.map ~f:fst
    (Set.Poly.filter ~f:(fun (_, trans) -> constrained trans)
       (parameter_set mir))

let list_multi_twiddles (mir : Program.Typed.t) :
  (string * Location_span.t Set.Poly.t) Set.Poly.t =
  let collect_twiddle_stmt (stmt : Stmt.Located.t) : (string, Location_span.t Set.Poly.t) Map.Poly.t =
    match stmt.pattern with
    | Stmt.Fixed.Pattern.TargetPE
        ({pattern=
            Expr.Fixed.Pattern.FunApp
              (_, _, (({pattern= Var vname; _})::_)); _})
      -> Map.Poly.singleton vname (Set.Poly.singleton stmt.meta)
    | _ -> Map.Poly.empty
  in
  let twiddles = fold_stmts
      ~take_stmt:(fun m s -> merge_set_maps m (collect_twiddle_stmt s))
      ~take_expr:(fun m _ -> m)
      ~init:Map.Poly.empty
      mir.log_prob
  in
  let multi_twiddles = Map.Poly.filter ~f:(fun s -> Set.Poly.length s <> 1) twiddles
  in
  Map.fold ~init:Set.Poly.empty ~f:(fun ~key ~data s -> Set.add s (key, data)) multi_twiddles

let list_param_dependant_cf (mir : Program.Typed.t)
  : (Location_span.t * string Set.Poly.t) Set.Poly.t =
  let params = parameter_names_set mir in
  let is_param v = Set.Poly.mem params v in
  let info_map = log_prob_build_dep_info_map mir in
  let cf_labels = Set.Poly.of_list
      (Map.Poly.keys
         (Map.Poly.filter info_map ~f:(fun (stmt, _) ->
              is_ctrl_flow stmt)))
  in
  let label_var_deps label : (Location_span.t * string Set.Poly.t) =
    let dep_labels = node_dependencies info_map label in
    let dep_exprs = union_map dep_labels ~f:(fun label ->
        let stmt, _ = Map.Poly.find_exn info_map label in
        stmt_rhs_var_set stmt)
    in
    let dep_vars = Set.Poly.map ~f:(fun (VVar v, _) -> v) dep_exprs in
    let dep_params = Set.Poly.inter params dep_vars in
    let _, info = Map.Poly.find_exn info_map label in
    (info.meta, dep_params)
  in
  let all_cf_var_deps = Set.Poly.map ~f:label_var_deps cf_labels in
  Set.Poly.filter ~f:(fun (_, vars) -> Set.Poly.exists ~f:is_param vars) all_cf_var_deps

let list_unscaled_constants (mir : Program.Typed.t)
  : (Location_span.t * string) Set.Poly.t =
  let collect_unscaled_expr (expr : Expr.Typed.t) =
    match expr.pattern with
    | Expr.Fixed.Pattern.Lit (Real, rstr)
    | Expr.Fixed.Pattern.Lit (Int, rstr) ->
      let mag = Float.abs (float_of_string rstr) in
      if (mag < 0.1 || mag > 10.0) && mag <> 0.0 then
        Set.Poly.singleton (expr.meta.loc, rstr)
      else
        Set.Poly.empty
    | _ -> Set.Poly.empty
  in
  fold_stmts
    ~take_stmt:(fun a _ -> a)
    ~take_expr:(fun a e -> Set.Poly.union a (collect_unscaled_expr e))
    ~init:Set.Poly.empty
    (List.concat [mir.log_prob; mir.generate_quantities; List.map ~f:(fun f -> f.fdbody) mir.functions_block])

let list_non_one_priors (mir : Program.Typed.t) : (string * int) Set.Poly.t =
  let priors = list_priors mir in
  let prior_set =
    Map.Poly.fold
      priors
      ~init:Set.Poly.empty
      ~f:(fun ~key:(VVar v) ~data:factors_opt s ->
          Option.value_map factors_opt ~default:s
            ~f:(fun factors -> Set.Poly.add s (v, Set.Poly.length factors)))
  in
  Set.Poly.filter prior_set ~f:(fun (_, n) -> n <> 1)

let warn_set (elems : 'a Set.Poly.t) (message : 'a -> string) =
  Set.Poly.iter elems ~f:(fun elem ->
      Out_channel.output_string stderr (message elem))

let print_warn_unscaled_constants (mir : Program.Typed.t) =
  let consts = list_unscaled_constants mir in
  let message (loc, name) =
    "Warning: At " ^ Location_span.to_string loc ^ ", you have the constant " ^ name ^ " which is less than 0.1 or more than 10 in absolute value. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example.\n"
  in warn_set consts message

let print_warn_sigma_unbounded (mir : Program.Typed.t) =
  let pnames = list_sigma_unbounded mir in
  let message pname =
    "Warning: Your Stan program has an unconstrained parameter \"" ^ pname ^ "\" whose name begins with \"sigma\". Parameters with this name are typically scale parameters and constrained to be positive. If this parameter is indeed a scale (or standard deviation or variance) parameter, add lower=0 to its declaration.\n"
  in warn_set pnames message

let print_warn_hard_constrained (mir : Program.Typed.t) =
  let pnames = list_hard_constrained mir in
  let message pname =
    "Warning: Your Stan program has a parameter \"" ^ pname ^ "\" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.\n"
  in warn_set pnames message

let print_warn_multi_twiddles (mir : Program.Typed.t) =
  let twds = list_multi_twiddles mir in
  let message (vname, _) =
    "Warning: The parameter " ^ vname ^ " is on the left-hand side of more than one twiddle statement.\n"
  in warn_set twds message

let print_warn_param_dependant_cf (mir : Program.Typed.t) =
  let cfs = list_param_dependant_cf mir in
  let message (loc, plist) =
    let plistStr = String.concat ~sep:", " (Set.Poly.to_list plist) in
    "Warning: The control flow statement at " ^ Location_span.to_string loc ^ " depends on parameter(s): " ^ plistStr ^ ".\n"
  in warn_set cfs message

let print_warn_unused_params (mir : Program.Typed.t) =
  let pnames = list_unused_params mir in
  let message pname =
    "Warning: The parameter " ^ pname ^ " was defined but never used.\n"
  in warn_set pnames message

let print_warn_non_one_priors (mir : Program.Typed.t) =
  let vars = list_non_one_priors mir in
  let message (pname, n) =
    "Warning: The parameter " ^ pname ^ " has " ^ string_of_int n ^ " priors.\n"
  in warn_set vars message

let print_warn_uninitialized (mir : Program.Typed.t) =
  let uninit_vars =
    Set.Poly.filter
      ~f:(fun (span, _) -> span <> Location_span.empty)
      (Dependence_analysis.mir_uninitialized_variables mir)
  in
  let message (span, var_name) =
    "Warning: the variable " ^ var_name ^ " may not have been initialized its use at " ^ Location_span.to_string span ^ ".\n"
  in
  warn_set uninit_vars message

let print_warn_distribution_warnings (mir : Program.Typed.t) =
  warn_set (list_distribution_warnings mir) ident

let print_warn_pedantic (mir : Program.Typed.t) =
  print_warn_sigma_unbounded mir;
  print_warn_unscaled_constants mir;
  print_warn_multi_twiddles mir;
  print_warn_hard_constrained mir;
  print_warn_unused_params mir;
  print_warn_param_dependant_cf mir;
  print_warn_non_one_priors mir;
  print_warn_uninitialized mir;
  print_warn_distribution_warnings mir;
