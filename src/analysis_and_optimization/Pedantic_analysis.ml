open Core_kernel
open Core_kernel.Poly
open Optimize
open Middle
open Middle.Program
open Dependence_analysis
open Dataflow_types
open Dataflow_utils
open Factor_graph
open Mir_utils
open Pedantic_dist_warnings

type warning_span = Location_span.t * string [@@deriving compare]

(*********************
   Pattern collection functions
 ********************)

let list_unused_params (factor_graph : factor_graph) (mir : Program.Typed.t) :
    string Set.Poly.t =
  (* Build a factor graph of the program, check for missing parameters *)
  let params = parameter_names_set ~include_transformed:false mir in
  let used_params =
    Set.Poly.map
      ~f:(fun (VVar v) -> v)
      (Set.Poly.of_list (Map.Poly.keys factor_graph.var_map)) in
  Set.Poly.diff params used_params

let list_hard_constrained (mir : Program.Typed.t) :
    (string * [`HardConstraint | `NonsenseConstraint]) Set.Poly.t =
  (* Iterate through all parameters' transformations for hard constraints *)
  let constrained (e : bound_values) =
    match e with
    | {lower= `Lit 0.; upper= `Lit 1.} | {lower= `Lit -1.; upper= `Lit 1.} ->
        None
    | {lower= `Lit a; upper= `Lit b} when a >= b -> Some `NonsenseConstraint
    | {lower= `Lit _; upper= `Lit _} -> Some `HardConstraint
    | _ -> None in
  Set.Poly.filter_map
    ~f:(fun (name, trans) ->
      Option.map
        ~f:(fun c -> (name, c))
        (constrained (trans_bounds_values trans)) )
    (parameter_set mir)

let list_multi_tildes (mir : Program.Typed.t) :
    (string * Location_span.t Set.Poly.t) Set.Poly.t =
  (* Collect statements of the form "target += Dist(param, ...)" *)
  let collect_tilde_stmt (stmt : Stmt.Located.t) :
      (string, Location_span.t Set.Poly.t) Map.Poly.t =
    match stmt.pattern with
    | Stmt.Fixed.Pattern.TargetPE
        {pattern= Expr.Fixed.Pattern.FunApp (_, {pattern= Var vname; _} :: _); _}
      ->
        Map.Poly.singleton vname (Set.Poly.singleton stmt.meta)
    | _ -> Map.Poly.empty in
  let tildes =
    fold_stmts
      ~take_stmt:(fun m s -> merge_set_maps m (collect_tilde_stmt s))
      ~take_expr:(fun m _ -> m)
      ~init:Map.Poly.empty mir.log_prob in
  (* Filter for parameters assigned more than one distribution *)
  let multi_tildes =
    Map.Poly.filter ~f:(fun s -> Set.Poly.length s <> 1) tildes in
  Map.fold ~init:Set.Poly.empty
    ~f:(fun ~key ~data s -> Set.add s (key, data))
    multi_tildes

(* Find all of the targets which are dependencies for a given label *)
let var_deps info_map label ?expr:(expr_opt : Expr.Typed.t option = None)
    (targets : string Set.Poly.t) : string Set.Poly.t =
  (* Labels of dependencies *)
  let dep_labels, expr_vars =
    match expr_opt with
    | None -> (node_dependencies info_map label, Set.Poly.empty)
    | Some expr ->
        let vvars = Set.Poly.map ~f:fst (expr_var_set expr) in
        ( node_vars_dependencies info_map vvars label
        , Set.Poly.map ~f:string_of_vexpr vvars ) in
  (* expressions of dependencies *)
  let dep_exprs =
    union_map dep_labels ~f:(fun label ->
        let stmt, _ = Map.Poly.find_exn info_map label in
        stmt_rhs_var_set stmt ) in
  (* variable dependencies *)
  let dep_vars = Set.Poly.map ~f:(fun (VVar v, _) -> v) dep_exprs in
  (* target dependencies *)
  Set.Poly.inter targets (Set.Poly.union dep_vars expr_vars)

let list_target_dependant_cf
    (info_map :
      ( label
      , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
      Map.Poly.t ) (targets : string Set.Poly.t) :
    (Location_span.t * string Set.Poly.t) Set.Poly.t =
  (* Find all the control flow nodes *)
  let cf_labels =
    Set.Poly.of_list
      (Map.Poly.keys
         (Map.Poly.filter info_map ~f:(fun (stmt, _) -> is_ctrl_flow stmt)) )
  in
  Set.Poly.filter_map
    ~f:(fun label ->
      let deps = var_deps info_map label targets in
      if Set.Poly.is_empty deps then None
      else
        let _, info = Map.Poly.find_exn info_map label in
        Some (info.meta, deps) )
    cf_labels

let list_param_dependant_cf (mir : Program.Typed.t) :
    (Location_span.t * string Set.Poly.t) Set.Poly.t =
  let params = parameter_names_set mir in
  (* build dataflow data structure *)
  let info_map = log_prob_build_dep_info_map mir in
  list_target_dependant_cf info_map params

let list_arg_dependant_fundef_cf (mir : Program.Typed.t)
    (fun_def : 'a Program.fun_def) : (Location_span.t * int * string) Set.Poly.t
    =
  let args = List.map ~f:(fun (_, name, _) -> name) fun_def.fdargs in
  (* Only look for control flow if this function definition has a body *)
  Option.value_map fun_def.fdbody ~default:Set.Poly.empty ~f:(fun body ->
      (* build dataflow data structure *)
      let info_map = build_dep_info_map mir body in
      let cf_deps = list_target_dependant_cf info_map (Set.Poly.of_list args) in
      union_map cf_deps ~f:(fun (loc, names) ->
          Set.Poly.map names ~f:(fun name ->
              let ix, _ =
                Option.value_exn
                  ~message:
                    "INTERNAL ERROR: Pedantic mode found CF dependent on an \
                     arg,but the arg is mismatched. Please report a bug.\n"
                  (List.findi args ~f:(fun _ arg -> arg = name)) in
              (loc, ix, name) ) ) )

let expr_collect_exprs (expr : Expr.Typed.t) ~f : 'a Set.Poly.t =
  let collect_expr s (expr : Expr.Typed.t) =
    match f expr with Some a -> Set.Poly.add s a | _ -> s in
  fold_expr ~init:Set.Poly.empty ~take_expr:(fun s e -> collect_expr s e) expr

let stmts_collect_exprs
    (stmts : (Expr.Typed.Meta.t, Stmt.Located.Meta.t) Stmt.Fixed.t List.t) ~f :
    'a Set.Poly.t =
  let collect_expr s (expr : Expr.Typed.t) =
    match f expr with Some a -> Set.Poly.add s a | _ -> s in
  fold_stmts ~init:Set.Poly.empty
    ~take_stmt:(fun s _ -> s)
    ~take_expr:(fun s e -> collect_expr s e)
    stmts

let list_param_dependant_fundef_cf (mir : Program.Typed.t)
    (info_map :
      ( label
      , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * node_dep_info )
      Map.Poly.t ) (fun_def : 'a Program.fun_def) :
    (Location_span.t * string Set.Poly.t * string * Location_span.t) Set.Poly.t
    =
  let dep_args = list_arg_dependant_fundef_cf mir fun_def in
  let fun_calls : (Expr.Typed.t * label) Set.Poly.t =
    Set.Poly.union_list
      (List.map ~f:snd
         (Map.Poly.to_alist
            (Map.Poly.filter_mapi info_map ~f:(fun ~key:label ~data:(stmt, _) ->
                 let funapps =
                   union_map (stmt_rhs stmt) ~f:(fun rhs_expr ->
                       expr_collect_exprs rhs_expr ~f:(fun rhs_subexpr ->
                           match rhs_subexpr.pattern with
                           | Expr.Fixed.Pattern.FunApp
                               (UserDefined (fname, _), _)
                             when fname = fun_def.fdname ->
                               Some (rhs_subexpr, label)
                           | _ -> None ) ) in
                 if Set.Poly.is_empty funapps then None else Some funapps ) ) ) )
  in
  let arg_exprs (fcall_expr : Expr.Typed.t) =
    match fcall_expr with
    | {pattern= Expr.Fixed.Pattern.FunApp (UserDefined (fname, _), arg_exprs); _}
      when fname = fun_def.fdname ->
        Set.Poly.map dep_args ~f:(fun (loc, ix, arg_name) ->
            (loc, List.nth_exn arg_exprs ix, arg_name) )
    | _ ->
        raise
          (Failure
             "In finding searching for parameter dependent functionarguments, \
              mismatched function. Please report a bug.\n" ) in
  let arg_param_deps label arg_expr =
    var_deps info_map ~expr:(Some arg_expr) label (parameter_names_set mir)
  in
  union_map fun_calls ~f:(fun (fcall_expr, label) ->
      Set.Poly.filter_map (arg_exprs fcall_expr)
        ~f:(fun (cf_loc, arg_expr, arg_name) ->
          let deps = arg_param_deps label arg_expr in
          if Set.Poly.is_empty deps then None
          else Some (cf_loc, deps, arg_name, arg_expr.meta.loc) ) )

let list_param_dependant_fundefs_cf (mir : Program.Typed.t) :
    (string * Location_span.t * string Set.Poly.t * string * Location_span.t)
    Set.Poly.t =
  let info_map = log_prob_build_dep_info_map mir in
  union_map (Set.Poly.of_list mir.functions_block) ~f:(fun fun_def ->
      let dependant_args = list_param_dependant_fundef_cf mir info_map fun_def in
      Set.Poly.map dependant_args ~f:(fun (cf_loc, deps, arg_name, arg_loc) ->
          (fun_def.fdname, cf_loc, deps, arg_name, arg_loc) ) )

let list_non_one_priors (fg : factor_graph) (mir : Program.Typed.t) :
    (string * int) Set.Poly.t =
  (* Use the factor graph definition of priors, which treats a neighboring
     factor as a prior for parameter P if it has no connection to the data
     except through P *)
  let priors = list_priors ~factor_graph:(Some fg) mir in
  let prior_set =
    Map.Poly.fold priors ~init:Set.Poly.empty
      ~f:(fun ~key:(VVar v) ~data:factors_opt s ->
        Option.value_map factors_opt ~default:s ~f:(fun factors ->
            Set.Poly.add s (v, Set.Poly.length factors) ) ) in
  (* Return only multi-prior parameters *)
  Set.Poly.filter prior_set ~f:(fun (_, n) -> n <> 1)

(* Collect useful information about an expression that's available at
   compile-time into a convenient form. *)
let compiletime_value_of_expr
    (params : (string * Expr.Typed.t Transformation.t) Set.Poly.t)
    (data : string Set.Poly.t) (expr : Expr.Typed.t) :
    compiletime_val * Expr.Typed.Meta.t =
  let v =
    match expr with
    | {pattern= Var pname; _} -> (
      match Set.Poly.find params ~f:(fun (name, _) -> name = pname) with
      | Some (name, trans) -> Param (name, trans)
      | None -> (
        match Set.Poly.find data ~f:(fun name -> name = pname) with
        | Some name -> Data name
        | None -> Opaque ) )
    | _ ->
        Option.value_map (num_expr_value expr) ~default:Opaque ~f:(fun (v, s) ->
            Number (v, s) ) in
  (v, expr.meta)

(* Scrape all distributions from the program by searching for their function
   names and function type, and wrangle some useful data about them, like the
   nature of their first argument
*)
let list_distributions (mir : Program.Typed.t) : dist_info Set.Poly.t =
  let take_dist (expr : Expr.Typed.t) =
    match expr.pattern with
    | Expr.Fixed.Pattern.FunApp (StanLib (fname, FnLpdf true, _), arg_exprs) ->
        let fname = chop_dist_name fname |> Option.value_exn in
        let params = parameter_set mir in
        let data = data_set mir in
        let args =
          List.map ~f:(compiletime_value_of_expr params data) arg_exprs in
        Some {name= fname; loc= expr.meta.loc; args}
    | _ -> None in
  stmts_collect_exprs
    (List.append mir.log_prob
       (List.filter_map ~f:(fun f -> f.fdbody) mir.functions_block) )
    ~f:take_dist

(* Our definition of 'unscaled' for constants used in distributions *)
let is_unscaled_value (v : float) =
  let mag = Float.abs v in
  (mag < 0.1 || mag > 10.0) && mag <> 0.0

let list_unscaled_constants (distributions_list : dist_info Set.Poly.t) :
    (Location_span.t * string) Set.Poly.t =
  (* Search all distributions for unscaled values *)
  let collect_unscaled_expr (arg : compiletime_val * Expr.Typed.Meta.t) =
    match arg with
    | Number (num, num_str), meta ->
        if is_unscaled_value num then Set.Poly.singleton (meta.loc, num_str)
        else Set.Poly.empty
    | _ -> Set.Poly.empty in
  union_map
    ~f:(fun {args; _} ->
      Set.Poly.union_list (List.map ~f:collect_unscaled_expr args) )
    distributions_list

(*********************
   Printing functions
 ********************)

let unscaled_constants_message (name : string) : string =
  Printf.sprintf
    "Argument %s suggests there may be parameters that are not unit scale; \
     consider rescaling with a multiplier (see manual section 22.12)."
    name

let unscaled_constants_warnings (distributions_list : dist_info Set.Poly.t) =
  Set.Poly.map
    ~f:(fun (loc, name) -> (loc, unscaled_constants_message name))
    (list_unscaled_constants distributions_list)

let nonsense_constrained_message (pname : string) : string =
  Printf.sprintf
    "Parameter %s has constraints that don't make sense. The lower bound \
     should be strictly less than the upper bound."
    pname

let hard_constrained_message (pname : string) : string =
  Printf.sprintf
    "Your Stan program has a parameter %s with a lower and upper bound in its \
     declaration. These hard constraints are not recommended, for two reasons: \
     (a) Except when there are logical or physical constraints, it is very \
     unusual for you to be sure that a parameter will fall inside a specified \
     range, and (b) The infinite gradient induced by a hard constraint can \
     cause difficulties for Stan's sampling algorithm. As a consequence, we \
     recommend soft constraints rather than hard constraints; for example, \
     instead of constraining an elasticity parameter to fall between 0, and 1, \
     leave it unconstrained and give it a normal(0.5,0.5) prior distribution."
    pname

let hard_constrained_warnings (mir : Program.Typed.t) =
  let pnames = list_hard_constrained mir in
  Set.Poly.map
    ~f:(fun (pname, c) ->
      match c with
      | `HardConstraint -> (Location_span.empty, hard_constrained_message pname)
      | `NonsenseConstraint ->
          (Location_span.empty, nonsense_constrained_message pname) )
    pnames

let multi_tildes_message (vname : string) : string =
  Printf.sprintf
    "The parameter %s is on the left-hand side of more than one tilde \
     statement."
    vname

let multi_tildes_warnings (mir : Program.Typed.t) =
  let twds = list_multi_tildes mir in
  Set.Poly.map
    ~f:(fun (vname, locs) ->
      (Set.Poly.min_elt_exn locs, multi_tildes_message vname) )
    twds

let param_dependant_cf_message (plist : string Set.Poly.t) : string =
  let plistStr = String.concat ~sep:", " (Set.Poly.to_list plist) in
  Printf.sprintf "A control flow statement depends on parameter(s): %s."
    plistStr

let param_dependant_cf_warnings (mir : Program.Typed.t) =
  let cfs = list_param_dependant_cf mir in
  Set.Poly.map
    ~f:(fun (loc, plist) -> (loc, param_dependant_cf_message plist))
    cfs

let param_dependant_fundef_cf_message (fname : string)
    (plist : string Set.Poly.t) (arg_name : string) (callsite : Location_span.t)
    : string =
  let plistStr = String.concat ~sep:", " (Set.Poly.to_list plist) in
  Printf.sprintf
    "A control flow statement inside function %s depends on argument %s. At \
     %s, the value of %s depends on parameter(s): %s."
    fname arg_name
    (Location_span.to_string callsite)
    arg_name plistStr

let param_dependant_fundef_cf_warnings (mir : Program.Typed.t) =
  Set.Poly.map
    ~f:(fun (fname, cf_loc, deps, arg_name, arg_loc) ->
      (cf_loc, param_dependant_fundef_cf_message fname deps arg_name arg_loc) )
    (list_param_dependant_fundefs_cf mir)

let unused_params_message (pname : string) : string =
  Printf.sprintf
    "The parameter %s was declared but was not used in the density calculation."
    pname

let unused_params_warnings (factor_graph : factor_graph) (mir : Program.Typed.t)
    =
  Set.Poly.map
    ~f:(fun pname -> (Location_span.empty, unused_params_message pname))
    (list_unused_params factor_graph mir)

let non_one_priors_message (pname : string) (n : int) : string =
  if n = 0 then Printf.sprintf "The parameter %s has no priors." pname
  else Printf.sprintf "The parameter %s has %d priors." pname n

let non_one_priors_warnings (factor_graph : factor_graph) (mir : Program.Typed.t)
    =
  Set.Poly.map
    ~f:(fun (pname, n) -> (Location_span.empty, non_one_priors_message pname n))
    (list_non_one_priors factor_graph mir)

let uninitialized_message (vname : string) : string =
  Printf.sprintf
    "The variable %s may not have been assigned a value before its use." vname

let uninitialized_warnings (mir : Program.Typed.t) =
  let uninit_vars =
    Set.Poly.filter
      ~f:(fun (span, _) -> span <> Location_span.empty)
      (Dependence_analysis.mir_uninitialized_variables mir) in
  Set.Poly.map
    ~f:(fun (loc, vname) -> (loc, uninitialized_message vname))
    uninit_vars

let to_list warning_set =
  Set.Poly.to_list warning_set
  |> List.sort ~compare:compare_warning_span
  |> List.rev

(* String-print uninitialized warnings
   In case a user wants only this warning *)
let warn_uninitialized mir = uninitialized_warnings mir |> to_list

(* Optimization settings for constant propagation and partial evaluation *)
let settings_constant_prop =
  { no_optimizations with
    constant_propagation= true
  ; copy_propagation= true
  ; partial_evaluation= true }

(* Collect all pedantic mode warnings, sorted, to stderr *)
let warn_pedantic (mir_unopt : Program.Typed.t) =
  (* Some warnings will be stronger when constants are propagated *)
  let mir =
    Optimize.optimization_suite ~settings:settings_constant_prop mir_unopt in
  (* Try to avoid recomputation by pre-building structures *)
  let distributions_info = list_distributions mir in
  let factor_graph = prog_factor_graph mir in
  Set.Poly.union_list
    [ uninitialized_warnings mir; unscaled_constants_warnings distributions_info
    ; multi_tildes_warnings mir; hard_constrained_warnings mir
    ; unused_params_warnings factor_graph mir; param_dependant_cf_warnings mir
    ; param_dependant_fundef_cf_warnings mir
    ; non_one_priors_warnings factor_graph mir
    ; distribution_warnings distributions_info ]
  |> to_list
