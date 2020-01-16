open Core_kernel
open Middle
open Middle.Program
open Middle.Expr
open Dependence_analysis
open Dataflow_types
open Dataflow_utils
open Factor_graph
open Mir_utils

let list_unused_params (mir : Program.Typed.t) : string Set.Poly.t =
  let params = parameter_set mir in
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
  Set.Poly.filter ~f:(fun v -> String.is_prefix ~prefix:"sigma" v)
    (parameter_set ~trans_predicate:not_lower_zero mir)

let list_hard_constrained (mir : Program.Typed.t) :
  string Set.Poly.t =
  let rec bound_value (v : Expr.Typed.t) = match v with
    | {pattern= Fixed.Pattern.Lit (Real, str); _}
    | {pattern= Fixed.Pattern.Lit (Int, str); _} -> Some (float_of_string str)
    | {pattern= Fixed.Pattern.FunApp (StanLib, "PMinus__", [v]); _} ->
      (match bound_value v with
       | Some v -> Some (-.v)
       | None -> None)
    | _ -> None
  in
  let constrained (e : Expr.Typed.t transformation) = match e with
    | LowerUpper (lower, upper) ->
      (match (bound_value lower, bound_value upper) with
       | (Some 0., Some 1.) | (Some -1., Some 1.) -> false
       | _ -> true
      )
    | _ -> false
  in
  parameter_set ~trans_predicate:constrained mir

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

let list_uniform (mir : Program.Typed.t) :
  (Location_span.t * string) Set.Poly.t =
  let collect_uniform_stmt (stmt : Stmt.Located.t) =
    match stmt.pattern with
    | Stmt.Fixed.Pattern.TargetPE
        ({pattern=
            Expr.Fixed.Pattern.FunApp
              (StanLib, "uniform_propto_log", (({pattern= Var vname; _})::_)); _})
      -> Set.Poly.singleton (stmt.meta, vname)
    | _ -> Set.Poly.empty
  in
  fold_stmts
    ~take_stmt:(fun l s -> Set.Poly.union l (collect_uniform_stmt s))
    ~take_expr:(fun l _ -> l)
    ~init:Set.Poly.empty
    mir.log_prob

let list_param_dependant_cf (mir : Program.Typed.t)
  : (Location_span.t * string Set.Poly.t) Set.Poly.t =
  let params = parameter_set mir in
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
  let uniforms = list_unscaled_constants mir in
  let message (loc, name) =
    "Warning: At " ^ Location_span.to_string loc ^ ", you have the constant " ^ name ^ " which is less than 0.1 or more than 10 in absolute value. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section *** of the manual for an example.\n"
  in warn_set uniforms message

let print_warn_uniform (mir : Program.Typed.t) =
  let uniforms = list_uniform mir in
  let message (loc, name) =
    "Warning: At " ^ Location_span.to_string loc ^ ", your Stan program has a uniform distribution on variable " ^ name ^ ". The uniform distribution is not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a uniform density can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of giving an elasticity parameter a uniform(0,1) distribution, try normal(0.5,0.5).\n"
  in warn_set uniforms message

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

let print_warn_pedantic (mir : Program.Typed.t) =
  print_warn_sigma_unbounded mir;
  print_warn_uniform mir;
  print_warn_unscaled_constants mir;
  print_warn_multi_twiddles mir;
  print_warn_hard_constrained mir;
  print_warn_unused_params mir;
  print_warn_param_dependant_cf mir;
  print_warn_non_one_priors mir;
  print_warn_uninitialized mir;
