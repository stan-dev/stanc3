open Std
open Optimize
open Middle
open Middle.Program
open Dependence_analysis
open Dataflow_types
open Dataflow_utils
open Factor_graph
open Mir_utils
open Pedantic_dist_warnings

type warning_span = Location_span.t * string

(********************* Pattern collection functions ********************)

let list_unused_params (factor_graph : factor_graph) (mir : Program.Typed.t) :
    (string * Location_span.t) Set.Poly.t =
  (* Build a factor graph of the program, check for missing parameters *)
  let param_info = parameter_set ~include_transformed:false mir in
  let params = Set.Poly.map ~f:fst3 param_info in
  let used_params =
    Set.Poly.map
      ~f:(fun (VVar v) -> v)
      (Set.Poly.of_list
         (VExprMap.to_list factor_graph.var_map |> List.map ~f:fst)) in
  let unused = Set.Poly.diff params used_params in
  Set.Poly.filter_map
    ~f:(fun (pname, _, loc) ->
      if Set.Poly.mem pname unused then Some (pname, loc) else None)
    param_info

let list_hard_constrained (mir : Program.Typed.t) :
    (string * [`HardConstraint | `NonsenseConstraint] * Location_span.t)
    Set.Poly.t =
  (* Iterate through all parameters' transformations for hard constraints *)
  let constrained (e : bound_values) =
    match e with
    | {lower= `Lit 0.; upper= `Lit 1.} | {lower= `Lit -1.; upper= `Lit 1.} ->
        None
    | {lower= `Lit a; upper= `Lit b} when a >= b -> Some `NonsenseConstraint
    | {lower= `Lit _; upper= `Lit _} -> Some `HardConstraint
    | _ -> None in
  Set.Poly.filter_map
    ~f:(fun (name, trans, loc) ->
      Option.map
        ~f:(fun c -> (name, c, loc))
        (constrained (trans_bounds_values trans)))
    (parameter_set mir)

let list_multi_tildes (mir : Program.Typed.t) :
    (string * Location_span.t Set.Poly.t) Set.Poly.t =
  (* Collect statements of the form "target += Dist(param, ...)" *)
  let collect_tilde_stmt (stmt : Stmt.Located.t) :
      Location_span.t Set.Poly.t String.Map.t =
    match stmt.pattern with
    | Stmt.Pattern.TargetPE
        {pattern= Expr.Pattern.FunApp (_, {pattern= Var vname; _} :: _); _} ->
        String.Map.singleton vname (Set.Poly.singleton stmt.meta)
    | _ -> String.Map.empty in
  let tildes =
    fold_stmts
      ~take_stmt:(fun m s ->
        merge_set_maps (module String.Map) m (collect_tilde_stmt s))
      ~take_expr:Fun.const ~init:String.Map.empty mir.log_prob in
  (* Filter for parameters assigned more than one distribution *)
  let multi_tildes =
    String.Map.filter ~f:(fun _ s -> Set.Poly.cardinal s <> 1) tildes in
  String.Map.fold ~init:Set.Poly.empty
    ~f:(fun ~key ~data s -> Set.Poly.add (key, data) s)
    multi_tildes

(** Collect statements of the form "target += Dist(param, ...)" where param has
    possibly been transformed non-linearly *)
let list_possible_nonlinear (mir : Program.Typed.t) : Location_span.t Set.Poly.t
    =
  (* These functions are linear if all of their arguments are *)
  let linear_fnames =
    Operator.(
      [Plus; PPlus; Minus; PMinus; PNot; Transpose] |> List.map ~f:to_string)
    @ [ "add"; "append_block"; "append_row"; "append_col"; "block"; "col"; "cols"
      ; "row"; "rows"; "diagonal"; "head"; "tail"; "minus"; "negative_infinity"
      ; "not_a_number"; "rep_matrix"; "rep_vector"; "rep_row_vector"
      ; "positive_infinity"; "segment"; "subtract"; "sum"; "to_vector"
      ; "to_row_vector"; "to_matrix"; "to_array_1d"; "to_array_2d"; "transpose"
      ]
    |> String.Set.of_list in
  (* A simple check of linearity of an expression. allow_var is used for
     expressions like a*b, where at most one of a and b can be a variable *)
  let rec is_linear allow_var Expr.{pattern; _} =
    match pattern with
    | Expr.Pattern.Var _ -> allow_var
    | Lit _ -> true
    | Indexed (e, _) | Promotion (e, _, _) -> is_linear allow_var e
    | TernaryIf (e1, e2, e3) ->
        is_linear allow_var e1 && is_linear allow_var e2
        && is_linear allow_var e3
    | FunApp (StanLib (name, _, _), args) ->
        is_linear_function allow_var name args
    | FunApp (CompilerInternal (FnMakeArray | FnMakeRowVec), args) ->
        List.for_all ~f:(is_linear allow_var) args
    | _ -> false
  and is_linear_function allow_var name (args : 'a Expr.t list) =
    match (name, args) with
    | _, _ when String.Set.mem name linear_fnames ->
        List.for_all ~f:(is_linear allow_var) args
    | _, _ when List.for_all ~f:(is_linear false) args ->
        (* A function of all constants is fine *) true
    | ("Times__" | "Divide__" | "IntDivide__"), [a; b] ->
        (* We require at least one of these operands to be a constant *)
        (is_linear allow_var a && is_linear false b)
        || (is_linear false a && is_linear allow_var b)
    | "fma", [a; b; c] ->
        (* Similar to above. Partial evaluation can create fmas where the user
           wrote Times *)
        is_linear allow_var c
        && ((is_linear allow_var a && is_linear false b)
           || (is_linear false a && is_linear allow_var b))
    | _ -> false in
  let maybe_nonlinear_tilde (stmt : Stmt.Located.t) =
    match stmt.pattern with
    (* a ~ foo(...) gets translated to target += foo_lpdf(a, ...) *)
    | Stmt.Pattern.TargetPE
        { pattern=
            Expr.Pattern.FunApp
              ( ( StanLib (_, (FnLpdf _ | FnLpmf _), _)
                | UserDefined (_, (FnLpdf _ | FnLpmf _)) )
              , e :: _ )
        ; _ }
      when not (is_linear true e) ->
        Set.Poly.singleton stmt.meta
    | _ -> Set.Poly.empty in
  let bad_tildes =
    fold_stmts
      ~take_stmt:(fun m s -> Set.Poly.union m (maybe_nonlinear_tilde s))
      ~take_expr:Fun.const ~init:Set.Poly.empty mir.log_prob in
  bad_tildes

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
    Set.Poly.union_map dep_labels ~f:(fun label ->
        let stmt, _ = LabelMap.find label info_map in
        stmt_rhs_names_set stmt) in
  (* variable dependencies *)
  let dep_vars = Set.Poly.map ~f:(fun (VVar v) -> v) dep_exprs in
  (* target dependencies *)
  Set.Poly.inter targets (Set.Poly.union dep_vars expr_vars)

let list_target_dependant_cf
    (info_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    (targets : string Set.Poly.t) :
    (Location_span.t * string Set.Poly.t) Set.Poly.t =
  (* Find all the control flow nodes *)
  let cf_labels =
    Set.Poly.of_list
      (List.map ~f:fst
         (LabelMap.to_list
            (LabelMap.filter info_map ~f:(fun _ (stmt, _) -> is_ctrl_flow stmt))))
  in
  Set.Poly.filter_map
    ~f:(fun label ->
      let deps = var_deps info_map label targets in
      if Set.Poly.is_empty deps then None
      else
        let _, info = LabelMap.find label info_map in
        Some (info.meta, deps))
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
      Set.Poly.union_map cf_deps ~f:(fun (loc, names) ->
          Set.Poly.map names ~f:(fun name ->
              let ix =
                match List.find_index args ~f:(String.equal name) with
                | Some v -> v
                | None ->
                    Common.ICE.internal_error
                      "Pedantic mode found CF dependent on an arg, but the arg \
                       is mismatched." [@coverage off] in
              (loc, ix, name))))

let expr_collect_exprs (expr : Expr.Typed.t) ~f : 'a Set.Poly.t =
  let collect_expr s (expr : Expr.Typed.t) =
    match f expr with Some a -> Set.Poly.add a s | _ -> s in
  fold_expr ~init:Set.Poly.empty ~take_expr:collect_expr expr

let stmts_collect_exprs (stmts : Stmt.Located.t List.t) ~f : 'a Set.Poly.t =
  let collect_expr s (expr : Expr.Typed.t) =
    match f expr with Some a -> Set.Poly.add a s | _ -> s in
  fold_stmts ~init:Set.Poly.empty ~take_stmt:Fun.const ~take_expr:collect_expr
    stmts

let list_param_dependant_fundef_cf (mir : Program.Typed.t)
    (info_map :
      ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t)
    (fun_def : 'a Program.fun_def) :
    (Location_span.t * string Set.Poly.t * string * Location_span.t) Set.Poly.t
    =
  let dep_args = list_arg_dependant_fundef_cf mir fun_def in
  let fun_calls : (Expr.Typed.t * label) Set.Poly.t =
    Set.Poly.union_list
      (List.map ~f:snd
         (LabelMap.to_list
            (LabelMap.filter_map info_map ~f:(fun label (stmt, _) ->
                 let funapps =
                   Set.Poly.union_map (stmt_rhs stmt) ~f:(fun rhs_expr ->
                       expr_collect_exprs rhs_expr ~f:(fun rhs_subexpr ->
                           match rhs_subexpr.pattern with
                           | Expr.Pattern.FunApp (UserDefined (fname, _), _)
                             when fname = fun_def.fdname ->
                               Some (rhs_subexpr, label)
                           | _ -> None)) in
                 if Set.Poly.is_empty funapps then None else Some funapps))))
  in
  let arg_exprs (fcall_expr : Expr.Typed.t) =
    match fcall_expr with
    | {pattern= Expr.Pattern.FunApp (UserDefined (fname, _), arg_exprs); _}
      when fname = fun_def.fdname ->
        Set.Poly.map
          ~f:(fun (loc, ix, arg_name) -> (loc, List.nth arg_exprs ix, arg_name))
          dep_args
    | _ ->
        Common.ICE.internal_error
          "In finding searching for parameter dependent function arguments, \
           mismatched function." [@coverage off] in
  let arg_param_deps label arg_expr =
    var_deps info_map ~expr:(Some arg_expr) label (parameter_names_set mir)
  in
  Set.Poly.union_map fun_calls ~f:(fun (fcall_expr, label) ->
      Set.Poly.filter_map
        ~f:(fun (cf_loc, arg_expr, arg_name) ->
          let deps = arg_param_deps label arg_expr in
          if Set.Poly.is_empty deps then None
          else Some (cf_loc, deps, arg_name, arg_expr.meta.loc))
        (arg_exprs fcall_expr))

let list_param_dependant_fundefs_cf (mir : Program.Typed.t) :
    (string * Location_span.t * string Set.Poly.t * string * Location_span.t)
    Set.Poly.t =
  let info_map = log_prob_build_dep_info_map mir in
  Set.Poly.union_map (Set.Poly.of_list mir.functions_block) ~f:(fun fun_def ->
      let dependant_args = list_param_dependant_fundef_cf mir info_map fun_def in
      Set.Poly.map dependant_args ~f:(fun (cf_loc, deps, arg_name, arg_loc) ->
          (fun_def.fdname, cf_loc, deps, arg_name, arg_loc)))

let list_non_one_priors (fg : factor_graph) (mir : Program.Typed.t) :
    (string * int * Location_span.t) Set.Poly.t =
  (* Use the factor graph definition of priors, which treats a neighboring
     factor as a prior for parameter P if it has no connection to the data
     except through P *)
  let priors = list_priors ~factor_graph:(Some fg) mir in
  let prior_set =
    VExprMap.fold priors ~init:Set.Poly.empty
      ~f:(fun ~key:(VVar v) ~data:(factors_opt, loc) s ->
        Option.value_map factors_opt ~default:s ~f:(fun factors ->
            Set.Poly.add (v, Set.Poly.cardinal factors, loc) s)) in
  (* Return only multi-prior parameters *)
  Set.Poly.filter prior_set ~f:(fun (_, n, _) -> n <> 1)

(* Collect useful information about an expression that's available at
   compile-time into a convenient form. *)
let compiletime_value_of_expr
    (params :
      (string * Expr.Typed.t Transformation.t * Location_span.t) Set.Poly.t)
    (data : string Set.Poly.t) (expr : Expr.Typed.t) :
    compiletime_val * Expr.Typed.Meta.t =
  let v =
    match expr with
    | {pattern= Var pname; _} -> (
        match
          Set.Poly.to_seq params |> Seq.find (fun (name, _, _) -> name = pname)
        with
        | Some (name, trans, _) -> Param (name, trans)
        | None -> (
            match
              Set.Poly.to_seq data |> Seq.find (fun name -> name = pname)
            with
            | Some name -> Data name
            | None -> Opaque))
    | _ ->
        Option.value_map (num_expr_value expr) ~default:Opaque ~f:(fun (v, s) ->
            Number (v, s)) in
  (v, expr.meta)

(* Scrape all distributions from the program by searching for their function
   names and function type, and wrangle some useful data about them, like the
   nature of their first argument *)
let list_distributions (mir : Program.Typed.t) : dist_info Set.Poly.t =
  let take_dist (expr : Expr.Typed.t) =
    match expr.pattern with
    | Expr.Pattern.FunApp
        (StanLib (fname, (FnLpdf true | FnLpmf true), _), arg_exprs) ->
        let fname = chop_dist_name fname |> Option.get in
        let params = parameter_set mir in
        let data = data_set mir in
        let args =
          List.map ~f:(compiletime_value_of_expr params data) arg_exprs in
        Some {name= fname; loc= expr.meta.loc; args}
    | _ -> None in
  stmts_collect_exprs
    (List.append mir.log_prob
       (List.filter_map ~f:(fun f -> f.fdbody) mir.functions_block))
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
    | Number (num, num_str), meta when is_unscaled_value num ->
        Set.Poly.singleton (meta.loc, num_str)
    | _ -> Set.Poly.empty in
  Set.Poly.union_map
    ~f:(fun {args; _} ->
      Set.Poly.union_list (List.map ~f:collect_unscaled_expr args))
    distributions_list

(********************* Printing functions ********************)

let unscaled_constants_message (name : string) : string =
  Printf.sprintf
    "Argument %s suggests there may be parameters that are not unit scale; \
     consider rescaling with a multiplier, see: \
     https://mc-stan.org/docs/stan-users-guide/efficiency-tuning.html#standardizing-predictors"
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
    ~f:(fun (pname, c, loc) ->
      match c with
      | `HardConstraint -> (loc, hard_constrained_message pname)
      | `NonsenseConstraint -> (loc, nonsense_constrained_message pname))
    pnames

let maybe_jacobian_adjustment_warnings (mir : Program.Typed.t) =
  let locations = list_possible_nonlinear mir in
  Set.Poly.map
    ~f:(fun loc ->
      ( loc
      , "Left-hand side of distribution statement (~) may contain a non-linear \
         transform of a parameter or local variable. If it does, you need to \
         include a target += statement with the log absolute determinant of \
         the Jacobian of the transform. You could also consider defining a \
         transformed parameter and using jacobian += in the transformed \
         parameters block." ))
    locations

let multi_tildes_message (vname : string) : string =
  Printf.sprintf
    "The parameter %s is on the left-hand side of more than one tilde \
     statement."
    vname

let multi_tildes_warnings (mir : Program.Typed.t) =
  let twds = list_multi_tildes mir in
  Set.Poly.map
    ~f:(fun (vname, locs) ->
      (Set.Poly.min_elt locs, multi_tildes_message vname))
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
      (cf_loc, param_dependant_fundef_cf_message fname deps arg_name arg_loc))
    (list_param_dependant_fundefs_cf mir)

let unused_params_message (pname : string) : string =
  Printf.sprintf
    "The parameter %s was declared but was not used in the density calculation."
    pname

let unused_params_warnings (factor_graph : factor_graph) (mir : Program.Typed.t)
    =
  Set.Poly.map
    ~f:(fun (pname, loc) -> (loc, unused_params_message pname))
    (list_unused_params factor_graph mir)

let non_one_priors_message (pname : string) (n : int) : string =
  if n = 0 then
    Printf.sprintf
      "The parameter %s has no priors. This means either no prior is provided, \
       or the prior(s) depend on data variables. In the later case, this may \
       be a false positive."
      pname
  else Printf.sprintf "The parameter %s has %d priors." pname n

let non_one_priors_warnings (factor_graph : factor_graph)
    (mir : Program.Typed.t) =
  Set.Poly.map
    ~f:(fun (pname, n, loc) -> (loc, non_one_priors_message pname n))
    (list_non_one_priors factor_graph mir)

let uninitialized_message (vname : string) : string =
  Printf.sprintf
    "The variable %s may not have been assigned a value before its first use."
    vname

let uninitialized_warnings (mir : Program.Typed.t) =
  let uninit_vars =
    Set.Poly.filter
      ~f:(fun (span, _) -> span <> Location_span.empty)
      (Dependence_analysis.mir_uninitialized_variables mir) in
  let vars = Hash_set.create 32 in
  let deduplicated =
    Set.Poly.filter_map uninit_vars ~f:(fun (loc, var) ->
        if Hash_set.mem vars var then None
        else (
          Hash_set.add vars var;
          Some (loc, var))) in
  Set.Poly.map
    ~f:(fun (loc, vname) -> (loc, uninitialized_message vname))
    deduplicated

(* The eigenvectors_sym/eigenvalues_sym pair on the same argument: each call
   runs its own full eigendecomposition, while the combined
   eigendecompose_sym primitive computes both from a single solver. *)
let eigh_pair_message : string =
  "The same argument is passed to both eigenvectors_sym and eigenvalues_sym. \
   Each call performs a full eigendecomposition of its argument; consider \
   computing both results from a single decomposition, e.g. \
   tuple(matrix, vector) e = eigendecompose_sym(A); with the eigenvectors in \
   e.1 and the eigenvalues in e.2. Compiling with --O1 or higher fuses \
   adjacent pairs like this automatically."

let eigh_pair_warnings (mir : Program.Typed.t) : warning_span Set.Poly.t =
  let unmergeable_call =
    Expr.Helpers.contains_fn_kind
      (function
        | UserDefined _ | StanLib (_, (FnTarget | FnRng), _) -> true
        | _ -> false)
      ~init:false in
  let stmts =
    List.append mir.log_prob
      (List.filter_map ~f:(fun f -> f.fdbody) mir.functions_block) in
  let take_eigh name (expr : Expr.Typed.t) =
    match expr.pattern with
    | Expr.Pattern.FunApp (StanLib (n, _, _), [arg])
      when String.equal n name && not (unmergeable_call arg) ->
        Some arg
    | _ -> None in
  let vec_args = stmts_collect_exprs stmts ~f:(take_eigh "eigenvectors_sym") in
  let val_args = stmts_collect_exprs stmts ~f:(take_eigh "eigenvalues_sym") in
  Set.Poly.filter_map vec_args ~f:(fun arg ->
      if Set.Poly.exists val_args ~f:(Expr.Typed.equal arg) then
        Some (arg.meta.loc, eigh_pair_message)
      else None)

let to_list warning_set =
  Set.Poly.to_list warning_set |> List.sort ~cmp:Stdlib.compare

(* String-print uninitialized warnings. In case a user wants only this
   warning *)
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
    ; multi_tildes_warnings mir; maybe_jacobian_adjustment_warnings mir
    ; hard_constrained_warnings mir; unused_params_warnings factor_graph mir
    ; param_dependant_cf_warnings mir; param_dependant_fundef_cf_warnings mir
    ; non_one_priors_warnings factor_graph mir
    ; eigh_pair_warnings mir
    ; distribution_warnings distributions_info ]
  |> to_list
