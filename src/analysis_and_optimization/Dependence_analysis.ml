open Std
open Middle
open Dataflow_types
open Mir_utils
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

(***********************************)
(* Dependency analysis & interface *)
(***********************************)

(** Dependency analysis: which statements in a block are affected by which other
    statements. Pedantic mode asks whether an [if] depends on a parameter; the
    factor graph asks which data and parameters feed each [target] term.

    The data structure is built in four layers, each a map keyed by [label], an
    [int] naming one MIR statement:
    + {b Statement map} ([Dataflow_utils.build_statement_map]): every statement
      gets a label in pre-order and is stored with the children replaced by the
      children's labels.
    + {b Control flow} ([Dataflow_utils.build_cf_graphs]): per label, the
      statements that can run just before, and the {e control parents}, the
      [if]/[while]/[for] nodes that decide whether the statement runs.
    + {b Reaching definitions} ([Monotone_framework.reaching_definitions_mfp]):
      per label, pairs [(variable, label')] meaning "the statement at [label']
      may be the last one to have assigned [variable]", keyed by name only.
    + {b Accesses} (this module): the elements each statement reads and writes,
      indices included, so that a definition of [theta[1]] can be ruled out as a
      source for a read of [theta[2]].

    {2 Running example}

    {[
      parameters { real a; }
      model {
        vector[2] theta;
        theta[1] = a;          // label 5
        theta[2] = 1;          // label 6
        if (theta[2] > 0) ...  // label 7
      }
    ]}

    Node 7 reads [theta[2]]; the definitions of [theta] reaching node 7 are the
    declaration and nodes 5 and 6. The element test drops node 5, which writes
    [theta[1]], so the dependencies of node 7 are the declaration and node 6,
    and the [if] is not reported as depending on the parameter [a]. A definition
    is also dropped when the element test allows only iterations in which the
    definition executes after the read (design §7.7). *)

(* Documented in the interface. *)
type node_dep_info =
  { predecessors: label Set.Poly.t
  ; parents: label Set.Poly.t
  ; reaching_defn_entry: reaching_defn Set.Poly.t
  ; reaching_defn_exit: reaching_defn Set.Poly.t
  ; loop: label option
  ; accesses: access list
  ; meta: Location_span.t }

type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

type dependency_graph = label Set.Poly.t LabelMap.t

(***********************************)
(* Element test: can two accesses  *)
(* name the same element?          *)
(***********************************)

let confused =
  Dependent {directions= Set.Poly.of_list [Lt; Eq; Gt]; distance= None}

(** The constant difference when both carry the same symbol or none. *)
let linear_difference (left : linear) (right : linear) : int option =
  match (left.symbol, right.symbol) with
  | None, None -> Some (left.const - right.const)
  | Some left_symbol, Some right_symbol
    when Expr.Typed.compare left_symbol right_symbol = 0 ->
      Some (left.const - right.const)
  | Some _, _ | None, Some _ -> None

(** The dependence between two single index expressions. *)
let point_dependence (source : point) (sink : point) : dependence =
  match (source, sink) with
  | Affine source_offset, Affine sink_offset -> (
      (* strong SIV (Goff, Kennedy and Tseng 1991 §3): [i1 + o1 = i2 + o2] iff
         [i2 - i1 = o1 - o2]; a shared symbol cancels *)
      match linear_difference source_offset sink_offset with
      | Some distance ->
          let direction =
            if distance = 0 then Eq else if distance > 0 then Lt else Gt in
          Dependent
            {directions= Set.Poly.singleton direction; distance= Some distance}
      | None -> confused)
  | Invariant source_offset, Invariant sink_offset -> (
      (* ZIV: with the same symbol the elements differ iff the constants do *)
      match linear_difference source_offset sink_offset with
      | Some 0 | None -> confused
      | Some _ -> Independent)
  | Affine _, Invariant _ | Invariant _, Affine _ | Varying _, _ | _, Varying _
    ->
      confused

(** The dependence that holds at both [merged] and [position]: [Independent] if
    either is, if the distances differ, or if the direction sets are disjoint.
*)
let intersect_dependence (merged : dependence) (position : dependence) :
    dependence =
  match (merged, position) with
  | Independent, (Independent | Dependent _) | Dependent _, Independent ->
      Independent
  | ( Dependent {directions= merged_directions; distance= merged_distance}
    , Dependent {directions= position_directions; distance= position_distance} )
    ->
      let directions = Set.Poly.inter merged_directions position_directions in
      let distances_differ =
        match (merged_distance, position_distance) with
        | Some merged_d, Some position_d -> merged_d <> position_d
        | _ -> false in
      if distances_differ || Set.Poly.is_empty directions then Independent
      else
        Dependent
          { directions
          ; distance= Option.first_some merged_distance position_distance }

(** Either dependence may hold (the join over several access pairs): directions
    are unioned, the distance survives only when both agree. *)
let union_dependence (left : dependence) (right : dependence) : dependence =
  match (left, right) with
  | Independent, other | other, Independent -> other
  | ( Dependent {directions= left_dirs; distance= left_d}
    , Dependent {directions= right_dirs; distance= right_d} ) ->
      let distance =
        match (left_d, right_d) with
        | Some a, Some b when a = b -> Some a
        | _ -> None in
      Dependent {directions= Set.Poly.union left_dirs right_dirs; distance}

(** The dependence between [source] and [sink] over every index position;
    [confused] if index counts differ; [Independent] only when never equal. *)
let access_dependence (source : access) (sink : access) : dependence =
  if List.length source.subs <> List.length sink.subs then confused
  else
    List.fold_left2 source.subs sink.subs ~init:confused
      ~f:(fun merged source_sub sink_sub ->
        (* a slice or multi-index at either position is [confused] *)
        let position =
          match (source_sub, sink_sub) with
          | Index.Single source_point, Index.Single sink_point ->
              point_dependence source_point sink_point
          | _ -> confused in
        intersect_dependence merged position)

(** The accesses to [var] whose kind satisfies [keep]. *)
let accesses_to (var : string) ~keep (accesses : access list) : access list =
  List.filter accesses ~f:(fun access ->
      String.equal access.var var && keep access.kind)

let accesses_reading var =
  accesses_to var ~keep:(function Read | Increment -> true | Write -> false)

(** The accesses in [accesses] that write [var]: kind [Write] or [Increment]. *)
let accesses_writing var =
  accesses_to var ~keep:(function Write | Increment -> true | Read -> false)

(***********************************)
(* Reaching definitions, pruned by *)
(* subscript                       *)
(***********************************)

(** Find all of the reaching definitions of a variable in an RD set *)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : string) :
    label Set.Poly.t =
  Set.Poly.map
    (Set.Poly.filter rds ~f:(fun (defined, _) -> String.equal defined var))
    ~f:snd

(** No loop encloses [loop], so one iteration is one execution of the body. *)
let outermost (statement_map : dep_info_map) (loop : label option) =
  match loop with
  | None -> true
  | Some label -> Option.is_none (snd (LabelMap.find label statement_map)).loop

(** [dep] restricted to the directions under which the access at [src] executes
    before the access at [dst] (Kennedy and Allen 2001, Definition 2.1): an
    earlier iteration always does, the same iteration only when [src] is
    lexically first. Labels are pre-order, so [src < dst] is lexical order.
    Valid only when the shared innermost loop is [outermost]. *)
let ordered_dependence ~(src : label) ~(dst : label) (dep : dependence) :
    dependence =
  match dep with
  | Independent -> Independent
  | Dependent {directions; distance} ->
      let allowed =
        if src < dst then Set.Poly.of_list [Lt; Eq] else Set.Poly.singleton Lt
      in
      let directions = Set.Poly.inter directions allowed in
      if Set.Poly.is_empty directions then Independent
      else Dependent {directions; distance}

(** The label of the analysed statement itself. [mir_reaching_definitions]
    records definitions from outside the statement (data, parameters, function
    arguments) at this label; the root is an [SList], a [For] or a function body
    and writes nothing a node reads, so a source at this label always means
    "defined outside". *)
let root_label : label = 1

(** The join of [access_dependence] over each source access paired with each
    sink access; two [Increment]s commute and are skipped (design §7.4).
    [confused] when either side has no recorded access, and when the two nodes
    do not share their innermost loop, because directions compare iterations of
    one loop. *)
let pair_dependence ~same_loop (sources : access list) (sinks : access list) :
    dependence =
  match (sources, sinks) with
  | [], _ | _, [] -> confused
  | _ -> (
      let joined =
        List.fold_left sources ~init:Independent ~f:(fun merged source ->
            List.fold_left sinks ~init:merged ~f:(fun merged sink ->
                match (source.kind, sink.kind) with
                | Increment, Increment -> merged
                | _ -> union_dependence merged (access_dependence source sink)))
      in
      match joined with
      | Dependent _ when not same_loop -> confused
      | Independent | Dependent _ -> joined)

(** Every label in [sources] whose accesses (selected by [src_accesses]) can
    name an element among [dst_accesses], with the raw dependence of the pair. A
    source at [root_label], or one not in the map, is a definition from outside
    the statement and is [confused]. *)
let element_edges (statement_map : dep_info_map) ~(dst : label)
    ~(sources : label Set.Poly.t) ~(src_accesses : access list -> access list)
    ~(dst_accesses : access list) : (label * dependence) list =
  let _, dst_info = LabelMap.find dst statement_map in
  List.filter_map (Set.Poly.to_list sources) ~f:(fun src ->
      match LabelMap.find_opt src statement_map with
      | Some (_, src_info) when src <> root_label -> (
          let same_loop = Option.equal Int.equal src_info.loop dst_info.loop in
          match
            pair_dependence ~same_loop
              (src_accesses src_info.accesses)
              dst_accesses
          with
          | Independent -> None
          | Dependent _ as dep -> Some (src, dep))
      | Some _ | None -> Some (src, confused))

(** [dep] as the transitive closure sees it: under an [outermost] loop only the
    directions in which [src] executes before [dst] survive. *)
let executes_first (statement_map : dep_info_map) ~(src : label) ~(dst : label)
    (dep : dependence) : dependence =
  let _, dst_info = LabelMap.find dst statement_map in
  if outermost statement_map dst_info.loop then ordered_dependence ~src ~dst dep
  else dep

(** The definitions of [var] reaching [dst] that may produce an element [dst]
    reads and can execute before [dst] does. *)
let pruned_reaching_defns (statement_map : dep_info_map) (dst : label)
    (var : string) : label Set.Poly.t =
  let _, info = LabelMap.find dst statement_map in
  element_edges statement_map ~dst
    ~sources:(reaching_defn_lookup info.reaching_defn_entry var)
    ~src_accesses:(accesses_writing var)
    ~dst_accesses:(accesses_reading var info.accesses)
  |> List.filter_map ~f:(fun (src, dep) ->
      match executes_first statement_map ~src ~dst dep with
      | Independent -> None
      | Dependent _ -> Some src)
  |> Set.Poly.of_list

let node_immediate_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (label : label) :
    label Set.Poly.t =
  let stmt, info = LabelMap.find label statement_map in
  let rhs_set = Set.Poly.map (stmt_rhs_var_set stmt) ~f:fst in
  let rhs_deps =
    Set.Poly.union_map
      (Set.Poly.diff rhs_set blockers)
      ~f:(pruned_reaching_defns statement_map label) in
  Set.Poly.union info.parents rhs_deps

(* This is doing an explicit graph traversal with edges defined by
   node_immediate_dependencies. *)
let rec node_dependencies_rec (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (label : label)
    (visited : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem label visited then visited
  else
    let visited' = Set.Poly.add label visited in
    let deps = node_immediate_dependencies statement_map ~blockers label in
    Set.Poly.fold deps ~init:visited' ~f:(node_dependencies_rec statement_map)

let node_dependencies (statement_map : dep_info_map) (label : label) :
    label Set.Poly.t =
  node_dependencies_rec statement_map label Set.Poly.empty

let node_vars_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (vars : string Set.Poly.t)
    (label : label) : label Set.Poly.t =
  let _, info = LabelMap.find label statement_map in
  let var_deps =
    Set.Poly.union_map
      (Set.Poly.diff vars blockers)
      ~f:(pruned_reaching_defns statement_map label) in
  Set.Poly.fold
    (Set.Poly.union info.parents var_deps)
    ~init:Set.Poly.empty
    ~f:(node_dependencies_rec statement_map ~blockers)

(* The strategy here is to write an update function on the whole dependency
   graph in terms of node_immediate_dependencies, and then to find a
   fixed-point. Since it's updating the dependencies for the whole graph at a
   time, it should be more efficient than doing a graph traversal for each
   node. *)
let all_node_dependencies (statement_map : dep_info_map) : dependency_graph =
  let immediate_map =
    LabelMap.mapi statement_map ~f:(fun label _ ->
        node_immediate_dependencies statement_map label) in
  let step_node label m =
    let immediate = LabelMap.find label immediate_map in
    let updated =
      Set.Poly.union
        (Set.Poly.union_map immediate ~f:(fun label -> LabelMap.find label m))
        immediate in
    Set.Poly.remove label updated in
  let step_map m = LabelMap.mapi m ~f:(fun label _ -> step_node label m) in
  let map_equal = LabelMap.equal ~cmp:Set.Poly.equal in
  let rec step_until_fixed m =
    let m' = step_map m in
    if map_equal m m' then m else step_until_fixed m' in
  step_until_fixed immediate_map

let mir_reaching_definitions (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    reaching_defn Set.Poly.t entry_exit LabelMap.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt stmt in
  let (module Flowgraph) = flowgraph in
  let rd_map =
    reaching_definitions_mfp mir (module Flowgraph) flowgraph_to_mir in
  let to_rd_set set =
    Set.Poly.map set ~f:(fun (name, label_opt) ->
        (name, Option.value label_opt ~default:1)) in
  LabelMap.map rd_map ~f:(fun {entry; exit} ->
      {entry= to_rd_set entry; exit= to_rd_set exit})

let all_labels
    (module Flowgraph : Monotone_framework_sigs.FLOWGRAPH
      with type labels = int) : int Set.Poly.t =
  let step set =
    Set.Poly.union set
      (Set.Poly.union_map set ~f:(fun l -> LabelMap.find l Flowgraph.successors))
  in
  let rec step_fix set =
    let next = step set in
    if Set.Poly.equal set next then set else step_fix next in
  step_fix Flowgraph.initials

let prog_rhs_variables
    (flowgraph_to_mir : Stmt.Located.Non_recursive.t LabelMap.t)
    (labels : int Set.Poly.t) : string Set.Poly.t =
  let label_vars label =
    Set.Poly.map ~f:fst
      (stmt_rhs_var_set (LabelMap.find label flowgraph_to_mir).pattern) in
  Set.Poly.union_map labels ~f:label_vars

let stmt_uninitialized_variables (exceptions : string Set.Poly.t)
    (stmt : Stmt.Located.t) : (Location_span.t * string) Set.Poly.t =
  let flowgraph, flowgraph_to_mir =
    Monotone_framework.forward_flowgraph_of_stmt ~flatten_loops:true stmt in
  let (module Flowgraph) = flowgraph in
  let labels = all_labels (module Flowgraph) in
  let all_variables = prog_rhs_variables flowgraph_to_mir labels in
  let initialized_vars_map =
    initialized_vars_mfp all_variables (module Flowgraph) flowgraph_to_mir in
  let uninitialized =
    LabelMap.fold initialized_vars_map ~init:Set.Poly.empty
      ~f:(fun ~key:label ~data:inits acc ->
        let stmt = LabelMap.find label flowgraph_to_mir in
        let rhs =
          Set.Poly.map
            ~f:(fun (name, Expr.Typed.Meta.{loc; _}) -> (loc, name))
            (stmt_rhs_var_set stmt.pattern) in
        let uninitialized (_, var) = not (Set.Poly.mem var inits.entry) in
        let uninitialized_set = Set.Poly.filter ~f:uninitialized rhs in
        Set.Poly.union acc uninitialized_set) in
  Set.Poly.filter uninitialized ~f:(fun (_, v) ->
      not (Set.Poly.mem v exceptions))

let mir_uninitialized_variables (mir : Program.Typed.t) :
    (Location_span.t * string) Set.Poly.t =
  let flag_variables = List.map ~f:Flag_vars.to_string Flag_vars.enumerate in
  let function_names = function_names mir in
  let data_vars = data_set ~exclude_transformed:true mir in
  let globals =
    Set.Poly.union function_names (Set.Poly.of_list flag_variables) in
  let parameters =
    Set.Poly.of_list
      (List.map ~f:fst3
         (List.filter
            ~f:(fun (_, _, Program.{out_block; _}) -> out_block = Parameters)
            mir.output_vars)) in
  let globals_data = Set.Poly.union globals data_vars in
  let check_against_data b =
    stmt_uninitialized_variables globals_data
      {pattern= SList b; meta= Location_span.empty} in
  let check_against_data_params b =
    let globals_data_prep = Set.Poly.union globals_data parameters in
    stmt_uninitialized_variables globals_data_prep
      (* prepend prepare_data to detect bad transformed data usages *)
      {pattern= SList (mir.prepare_data @ b); meta= Location_span.empty} in
  Set.Poly.union_list
    [ (* prepare_data scope: data *)
      check_against_data mir.prepare_data
      (* log_prob scope: data, prep declarations *)
    ; check_against_data_params mir.log_prob
    ; check_against_data_params mir.reverse_mode_log_prob
      (* gen quant scope: data, prep declarations *)
    ; check_against_data_params mir.generate_quantities
      (* functions scope: arguments *)
    ; Set.Poly.union_list
        (List.map mir.functions_block ~f:(fun Program.{fdbody; fdargs; _} ->
             let arg_vars =
               Set.Poly.of_list
                 (List.map fdargs ~f:(fun (_, arg_name, _) -> arg_name)) in
             Option.value_map fdbody ~default:Set.Poly.empty ~f:(fun fdbody ->
                 stmt_uninitialized_variables
                   (Set.Poly.union arg_vars globals)
                   fdbody))) ]

(***********************************)
(* Building the map                *)
(***********************************)

(** [left ± right]; [None] when [linear] cannot hold the result. *)
let linear_combine ~negate_right (left : linear) (right : linear) :
    linear option =
  let const =
    if negate_right then left.const - right.const else left.const + right.const
  in
  match (left.symbol, right.symbol) with
  | symbol, None -> Some {const; symbol}
  | None, Some _ when not negate_right -> Some {const; symbol= right.symbol}
  | Some left_symbol, Some right_symbol
    when negate_right && Expr.Typed.compare left_symbol right_symbol = 0 ->
      Some {const; symbol= None}
  | None, Some _ | Some _, Some _ -> None

(** [left ± right] when the result is again [Affine] or [Invariant]. *)
let point_combine ~negate_right (left : point) (right : point) : point option =
  let combined wrap left_offset right_offset =
    Option.map (linear_combine ~negate_right left_offset right_offset) ~f:wrap
  in
  match (left, right) with
  | Invariant left_offset, Invariant right_offset ->
      combined (fun offset -> Invariant offset) left_offset right_offset
  | Affine left_offset, Invariant right_offset ->
      combined (fun offset -> Affine offset) left_offset right_offset
  | Invariant left_offset, Affine right_offset when not negate_right ->
      combined (fun offset -> Affine offset) left_offset right_offset
  | Affine left_offset, Affine right_offset when negate_right ->
      (* [(n + o1) - (n + o2)]: the loop variable cancels *)
      combined (fun offset -> Invariant offset) left_offset right_offset
  | Invariant _, Affine _ | Affine _, Affine _ | Varying _, _ | _, Varying _ ->
      None

(** Whether [expr] mentions at least one variable in [names]. *)
let mentions (names : string Set.Poly.t) (expr : Expr.Typed.t) =
  not (Set.Poly.disjoint (expr_var_names_set expr) names)

(** The [point] of [expr] with respect to [loopvar], read through [+], [-] and
    promotions; every other sub-expression is one symbol. *)
let classify_point ~(loopvar : string option)
    ~(written_vars : string Set.Poly.t) (expr : Expr.Typed.t) : point =
  let written_vars =
    Option.value_map loopvar ~default:written_vars ~f:(fun loop_variable ->
        Set.Poly.remove loop_variable written_vars) in
  let mentions_loopvar expr =
    Option.value_map loopvar ~default:false ~f:(fun loop_variable ->
        mentions (Set.Poly.singleton loop_variable) expr) in
  let rec classify (expr : Expr.Typed.t) : point =
    (* [expr] as one opaque symbol when invariant, else why [expr] varies *)
    let symbolic () =
      if mentions written_vars expr then Varying Written
      else if mentions_loopvar expr then Varying Nonlinear
      else Invariant {const= 0; symbol= Some expr} in
    let combine ~negate_right lhs rhs =
      match point_combine ~negate_right (classify lhs) (classify rhs) with
      | Some point -> point
      | None -> symbolic () in
    match expr.pattern with
    | Var name when Option.equal String.equal loopvar (Some name) ->
        Affine {const= 0; symbol= None}
    | Lit (Int, digits) -> (
        match Int.of_string_opt digits with
        | Some const -> Invariant {const; symbol= None}
        | None -> symbolic ())
    | Promotion (inner, _, _) -> classify inner
    | FunApp (Operator Plus, [lhs; rhs]) -> combine ~negate_right:false lhs rhs
    | FunApp (Operator Minus, [lhs; rhs]) -> combine ~negate_right:true lhs rhs
    | Var _ | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ ->
        symbolic () in
  classify expr

(** [index] with each index expression replaced by the expression's [point], so
    every Stan index kind ([e], [:], [a:], [a:b], [idxs]) is kept as written. *)
let classify_subscript ~loopvar ~written_vars (index : Expr.Typed.t Index.t) :
    point Index.t =
  Index.map (classify_point ~loopvar ~written_vars) index

(** Every variable reference inside [expr], in evaluation order; [target()]
    counts as a read of ["target"]; reads of the loop variable are omitted. *)
let rec reads_in_expr ~loopvar ~written_vars ~label (expr : Expr.Typed.t) :
    access list =
  let reads_in = reads_in_expr ~loopvar ~written_vars ~label in
  let reads_in_all exprs = List.concat_map exprs ~f:reads_in in
  match expr.pattern with
  | Var name when Option.equal String.equal loopvar (Some name) -> []
  | Var name -> [{var= name; subs= []; kind= Read; label}]
  | Lit _ -> []
  | Indexed ({pattern= Var name; _}, indices) ->
      let subs =
        List.map indices ~f:(classify_subscript ~loopvar ~written_vars) in
      {var= name; subs; kind= Read; label}
      :: reads_in_all (List.concat_map indices ~f:Index.bounds)
  | Indexed (base, indices) ->
      reads_in base @ reads_in_all (List.concat_map indices ~f:Index.bounds)
  | FunApp ((StanLib (_, FnTarget, _) | UserDefined (_, FnTarget)), args) ->
      {var= "target"; subs= []; kind= Read; label} :: reads_in_all args
  | FunApp (kind, args) -> reads_in_all (Fun_kind.collect_exprs kind @ args)
  | TernaryIf (cond, then_expr, else_expr) ->
      reads_in_all [cond; then_expr; else_expr]
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> reads_in_all [lhs; rhs]
  | Promotion (inner, _, _) | TupleProjection (inner, _) -> reads_in inner

(** The innermost [For] or [While] enclosing [label], found by climbing the
    control parents of [build_cf_graphs]. *)
let rec enclosing_loop statement_map parents (label : label) : label option =
  let pattern_of node = fst (LabelMap.find node statement_map) in
  let ctrl_parent =
    List.find_opt
      (Set.Poly.to_list (LabelMap.find label parents))
      ~f:(fun parent -> is_ctrl_flow (pattern_of parent)) in
  match ctrl_parent with
  | None -> None
  | Some parent -> (
      match pattern_of parent with
      | Stmt.Pattern.For _ | While _ -> Some parent
      | _ -> enclosing_loop statement_map parents parent)

(** The loop variable of the [For] at [loop]; [None] for a [While] or no loop.
*)
let loopvar_of statement_map (loop : label option) : string option =
  Option.bind loop ~f:(fun label ->
      match fst (LabelMap.find label statement_map) with
      | Stmt.Pattern.For {loopvar; _} -> Some loopvar
      | _ -> None)

(** The accesses of the statement at [label] alone, reads before the write;
    substatements are not visited, so an [if] or a loop contributes only the
    condition or bounds. [target += e] is one [Increment]. *)
let node_accesses ~loopvar ~written_vars ~label
    (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) : access list =
  let reads_in = reads_in_expr ~loopvar ~written_vars ~label in
  let reads_in_all exprs = List.concat_map exprs ~f:reads_in in
  match stmt with
  | Assignment ((LVariable name, indices), _, rhs) ->
      let subs =
        List.map indices ~f:(classify_subscript ~loopvar ~written_vars) in
      reads_in_all (List.concat_map indices ~f:Index.bounds)
      @ reads_in rhs
      @ [{var= name; subs; kind= Write; label}]
  | Assignment (((LTupleProjection _, _) as lhs), _, rhs) ->
      reads_in_all
        (List.concat_map (Stmt.Helpers.lhs_indices lhs) ~f:Index.bounds)
      @ reads_in rhs
      @ [{var= Stmt.Helpers.lhs_variable lhs; subs= []; kind= Write; label}]
  | Decl {decl_id; initialize= Assign init; _} ->
      reads_in init @ [{var= decl_id; subs= []; kind= Write; label}]
  | Decl {decl_id; _} -> [{var= decl_id; subs= []; kind= Write; label}]
  | TargetPE operand | JacobianPE operand ->
      reads_in operand @ [{var= "target"; subs= []; kind= Increment; label}]
  | Return (Some value) -> reads_in value
  | NRFunApp (kind, args) -> reads_in_all (Fun_kind.collect_exprs kind @ args)
  | IfElse (cond, _, _) | While (cond, _) -> reads_in cond
  | For {loopvar= inner; lower; upper; _} ->
      reads_in_all [lower; upper] @ [{var= inner; subs= []; kind= Write; label}]
  | Profile _ | Block _ | SList _ | Break | Continue | Skip | Return None -> []

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    dep_info_map =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = mir_reaching_definitions mir stmt in
  (* a symbol outside the written set has one value for the whole statement *)
  let written_vars =
    LabelMap.fold statement_map ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:(pattern, _) written ->
        Set.Poly.union written (assigned_or_declared_vars_stmt pattern)) in
  let loops =
    LabelMap.mapi statement_map ~f:(fun label _ ->
        enclosing_loop statement_map parents label) in
  LabelMap.mapi statement_map ~f:(fun label (pattern, meta) ->
      let rds = LabelMap.find label rd_map in
      let loop = LabelMap.find label loops in
      ( pattern
      , { predecessors= LabelMap.find label preds
        ; parents= LabelMap.find label parents
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit
        ; loop
        ; accesses=
            node_accesses
              ~loopvar:(loopvar_of statement_map loop)
              ~written_vars ~label pattern
        ; meta } ))

let log_prob_build_dep_info_map (mir : Program.Typed.t) : dep_info_map =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let log_prob_dependency_graph (mir : Program.Typed.t) : dependency_graph =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map

(***********************************)
(* Queries                         *)
(***********************************)

(** The variables read by the statements at [labels], where [Mir_utils.stmt_rhs]
    defines which positions of a statement count as reads. *)
let rhs_variables_at (statement_map : dep_info_map) (labels : label Set.Poly.t)
    : string Set.Poly.t =
  Set.Poly.union_map labels ~f:(fun label ->
      stmt_rhs_names_set (fst (LabelMap.find label statement_map)))
