open Core_kernel
open Optimize
open Middle
open Middle.Program
open Dependence_analysis
open Dataflow_types
open Dataflow_utils
open Factor_graph
open Mir_utils
open Pedantic_dist_warnings

let list_unused_params (factor_graph:factor_graph) (mir : Program.Typed.t) : string Set.Poly.t =
  let params = parameter_names_set mir in
  let used_params =
    Set.Poly.map
      ~f:(fun (VVar v) -> v)
      (Set.Poly.of_list (Map.Poly.keys factor_graph.var_map))
  in
  Set.Poly.diff params used_params

let list_hard_constrained (mir : Program.Typed.t) :
  string Set.Poly.t =
  let constrained (e : bound_values) = match e with
    | {lower= `Lit 0.; upper= `Lit 1.}
    | {lower= `Lit -1.; upper= `Lit 1.} -> false
    | {lower= `Lit _; upper= `Lit _} -> true
    | _ -> false
  in
  Set.Poly.map ~f:fst
    (Set.Poly.filter ~f:(fun (_, trans) -> constrained (trans_bounds_values trans))
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
  let twiddles =
    fold_stmts
      ~take_stmt:(fun m s -> merge_set_maps m (collect_twiddle_stmt s))
      ~take_expr:(fun m _ -> m)
      ~init:Map.Poly.empty
      mir.log_prob
  in
  let multi_twiddles =
    Map.Poly.filter ~f:(fun s -> Set.Poly.length s <> 1) twiddles
  in
  Map.fold
    ~init:Set.Poly.empty
    ~f:(fun ~key ~data s -> Set.add s (key, data))
    multi_twiddles

let list_param_dependant_cf (mir : Program.Typed.t)
  : (Location_span.t * string Set.Poly.t) Set.Poly.t =
  let params = parameter_names_set mir in
  (* build dataflow data structure *)
  let info_map = log_prob_build_dep_info_map mir in
  (* Find all the control flow nodes *)
  let cf_labels = Set.Poly.of_list
      (Map.Poly.keys
         (Map.Poly.filter info_map ~f:(fun (stmt, _) ->
              is_ctrl_flow stmt)))
  in
  (* Find all of the parameters which are dependencies for a given label *)
  let label_var_deps label : (Location_span.t * string Set.Poly.t) =
    (* Labels of dependencies *)
    let dep_labels = node_dependencies info_map label in
    (* expressions of dependencies *)
    let dep_exprs =
      union_map dep_labels ~f:(fun label ->
          let stmt, _ = Map.Poly.find_exn info_map label in
          stmt_rhs_var_set stmt)
    in
    (* variable dependencies *)
    let dep_vars = Set.Poly.map ~f:(fun (VVar v, _) -> v) dep_exprs in
    (* parameter dependencies *)
    let dep_params = Set.Poly.inter params dep_vars in
    let _, info = Map.Poly.find_exn info_map label in
    (info.meta, dep_params)
  in
  let all_cf_var_deps = Set.Poly.map ~f:label_var_deps cf_labels in
  (* remove empty dependency sets *)
  Set.Poly.filter ~f:(fun (_, vars) -> not (Set.Poly.is_empty vars)) all_cf_var_deps

let list_non_one_priors (fg : factor_graph) (mir : Program.Typed.t) : (string * int) Set.Poly.t =
  let priors = list_priors ~factor_graph:(Some fg) mir in
  let prior_set =
    Map.Poly.fold
      priors
      ~init:Set.Poly.empty
      ~f:(fun ~key:(VVar v) ~data:factors_opt s ->
          Option.value_map factors_opt ~default:s
            ~f:(fun factors -> Set.Poly.add s (v, Set.Poly.length factors)))
  in
  Set.Poly.filter prior_set ~f:(fun (_, n) -> n <> 1)

let compiletime_value_of_expr
    (params : (string * Expr.Typed.t Program.transformation) Set.Poly.t)
    (data : string Set.Poly.t)
    (expr : Expr.Typed.t) : (compiletime_val * Expr.Typed.Meta.t) =
  let v = match expr with
    | ({pattern= Var pname; _}) ->
      (match (Set.Poly.find params ~f:(fun (name, _) -> name = pname)) with
       | Some (name, trans) -> Param (name, trans_bounds_values trans)
       | None ->
         (match (Set.Poly.find data ~f:(fun name -> name = pname)) with
          | Some name -> Data name
          | None -> Opaque))
    | _ ->
      Option.value_map
        (num_expr_value expr)
        ~default:Opaque
        ~f:(fun (v, s) -> Number (v, s))
  in
  (v, expr.meta)

(* Scrape all distributions from the program by searching for their function
   names and function type, and wrangle some useful data about them, like the
   nature of their first argument
*)
let list_distributions (mir : Program.Typed.t) : dist_info Set.Poly.t =
  let collect_distribution_expr s (expr : Expr.Typed.Meta.t Expr.Fixed.t) =
    match expr.pattern with
    | Expr.Fixed.Pattern.FunApp
        (StanLib, fname, arg_exprs) ->
      (match chop_dist_name fname with
       | Some dname ->
         let params = parameter_set mir in
         let data = data_set mir in
         let args = List.map ~f:(compiletime_value_of_expr params data) arg_exprs in
         Set.Poly.add s
           { name = dname
           ; loc = expr.meta.loc
           ; args = args
           }
       | _ -> s)
    | _ -> s
  in
  fold_stmts
    ~init:Set.Poly.empty
    ~take_stmt:(fun s _ -> s)
    ~take_expr:(fun s e -> collect_distribution_expr s e)
    (List.append
       mir.log_prob
       (List.map ~f:(fun f -> f.fdbody) mir.functions_block))

let list_unscaled_constants (distributions_list : dist_info Set.Poly.t)
  : (Location_span.t * string) Set.Poly.t =
  let collect_unscaled_expr (arg : (compiletime_val * Expr.Typed.Meta.t)) = match arg with
    | (Number (num, num_str), meta) ->
      let mag = Float.abs num in
      if (mag < 0.1 || mag > 10.0) && mag <> 0.0 then
        Set.Poly.singleton (meta.loc, num_str)
      else
        Set.Poly.empty
    | _ -> Set.Poly.empty
  in
  union_map
    ~f:(fun {args;_} -> Set.Poly.union_list (List.map ~f:collect_unscaled_expr args))
    distributions_list

(*****
   Printing functions
 *****)

let warn_set (elems : 'a Set.Poly.t) (message : 'a -> string) =
  Set.Poly.iter elems ~f:(fun elem ->
      Out_channel.output_string stderr (message elem))

let warn_unscaled_constants (distributions_list : dist_info Set.Poly.t) =
  let consts = list_unscaled_constants distributions_list in
  let message (loc, name) =
    (loc, "A distribution argument " ^ name ^ " is less than 0.1 or more than 10 in magnitude. This suggests that you might have parameters in your model that have not been scaled to roughly order 1. We suggest rescaling using a multiplier; see section 22.12 of the manual for an example.\n")
  in Set.Poly.map ~f:message consts

let warn_hard_constrained (mir : Program.Typed.t) =
  let pnames = list_hard_constrained mir in
  let message pname =
    (Location_span.empty, "Your Stan program has a parameter \"" ^ pname ^ "\" with hard constraints in its declaration. Hard constraints are not recommended, for two reasons: (a) Except when there are logical or physical constraints, it is very unusual for you to be sure that a parameter will fall inside a specified range, and (b) The infinite gradient induced by a hard constraint can cause difficulties for Stan's sampling algorithm. As a consequence, we recommend soft constraints rather than hard constraints; for example, instead of constraining an elasticity parameter to fall between 0, and 1, leave it unconstrained and give it a normal(0.5,0.5) prior distribution.\n")
  in Set.Poly.map ~f:message pnames

let warn_multi_twiddles (mir : Program.Typed.t) =
  let twds = list_multi_twiddles mir in
  let message (vname, loc) =
    (Set.Poly.min_elt_exn loc, "The parameter " ^ vname ^ " is on the left-hand side of more than one twiddle statement.\n")
  in Set.Poly.map ~f:message twds

let warn_param_dependant_cf (mir : Program.Typed.t) =
  let cfs = list_param_dependant_cf mir in
  let message (loc, plist) =
    let plistStr = String.concat ~sep:", " (Set.Poly.to_list plist) in
    (loc, "A control flow statement depends on parameter(s): " ^ plistStr ^ ".\n")
  in Set.Poly.map ~f:message cfs

let warn_unused_params (factor_graph:factor_graph) (mir : Program.Typed.t) =
  let pnames = list_unused_params factor_graph mir in
  let message pname =
    (Location_span.empty, "The parameter " ^ pname ^ " was declared but does not participate in the model.\n")
  in Set.Poly.map ~f:message pnames

let warn_non_one_priors (factor_graph:factor_graph) (mir : Program.Typed.t) =
  let vars = list_non_one_priors factor_graph mir in
  let message (pname, n) =
    (Location_span.empty, "The parameter " ^ pname ^ " has " ^ string_of_int n ^ " priors.\n")
  in Set.Poly.map ~f:message vars


let warn_uninitialized (mir : Program.Typed.t) =
  let uninit_vars =
    Set.Poly.filter
      ~f:(fun (span, _) -> span <> Location_span.empty)
      (Dependence_analysis.mir_uninitialized_variables mir)
  in
  let message (span, var_name) =
    (span, "The variable " ^ var_name ^ " may not have been initialized before its use.\n")
  in
  Set.Poly.map ~f:message uninit_vars

let print_warn_uninitialized mir = warn_set (warn_uninitialized mir) snd

let settings_constant_prop =
  { function_inlining= false
  ; static_loop_unrolling= false
  ; one_step_loop_unrolling= false
  ; list_collapsing= false
  ; block_fixing= false
  ; constant_propagation= true
  ; expression_propagation= false
  ; copy_propagation= true
  ; dead_code_elimination= false
  ; partial_evaluation= true
  ; lazy_code_motion= false
  ; optimize_ad_levels= false }

let print_warn_pedantic (mir_unopt : Program.Typed.t) =
  (* Some warnings will be stronger when constants are propagated *)
  let mir =
    Optimize.optimization_suite
      ~optimization_settings:settings_constant_prop
      mir_unopt
  in
  let distributions_info = list_distributions mir in
  let factor_graph = prog_factor_graph mir in
  let warning_set =
    Set.Poly.union_list [
      warn_uninitialized mir
    ; warn_unscaled_constants distributions_info
    ; warn_multi_twiddles mir
    ; warn_hard_constrained mir
    ; warn_unused_params factor_graph mir
    ; warn_param_dependant_cf mir
    ; warn_non_one_priors factor_graph mir
    ; list_distribution_warnings distributions_info
    ]
  in
  let warn (loc, msg) =
    if loc = Location_span.empty then
      "Warning:\n  " ^ msg
    else
      "Warning at " ^ Location_span.to_string loc ^ ":\n  " ^ msg
  in
  warn_set warning_set warn
