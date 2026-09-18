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

    Four layers, each a map keyed by [label], an [int] naming one MIR statement:
    + {b Statement map} ([Dataflow_utils.build_statement_map]): every statement
      in pre-order, children replaced by the children's labels;
    + {b Control flow} ([Dataflow_utils.build_cf_graphs]): the statements that
      can run just before, and the {e control parents} ([if], [while], [for])
      that decide whether the statement runs;
    + {b Reaching definitions} ([Monotone_framework.reaching_definitions_mfp]):
      pairs [(variable, label')], "the statement at [label'] may be the last to
      have assigned [variable]", keyed by name only;
    + {b Accesses} (this module): the elements each statement reads and writes,
      indices included, so a definition of [theta[1]] is ruled out as a source
      for a read of [theta[2]].

    Running example, labels in the margin:
    {[
      theta[1] = a;          // 5
      theta[2] = 1;          // 6
      if (theta[2] > 0) ...  // 7
    ]}
    Node 7 reads [theta[2]]; the definitions of [theta] reaching node 7 are the
    declaration and nodes 5 and 6. The element test drops node 5, which writes
    [theta[1]], so the [if] is not reported as depending on the parameter [a]. A
    definition is also dropped when the element test allows only iterations in
    which the definition executes after the read (design §7.7). *)

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

(* ---- Element test: can two accesses name the same element? ---- *)

let confused =
  Dependent {directions= Set.Poly.of_list [Lt; Eq; Gt]; distance= None}

(** The constant difference when both carry the same symbol or none. *)
let linear_difference (left : linear) (right : linear) : int option =
  Option.some_if
    (Option.equal Expr.Typed.equal left.symbol right.symbol)
    (left.const - right.const)

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
  | Independent, _ | _, Independent -> Independent
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

(** Either may hold: directions unioned, the distance kept only when equal. *)
let union_dependence (left : dependence) (right : dependence) : dependence =
  match (left, right) with
  | Independent, other | other, Independent -> other
  | ( Dependent {directions= left_dirs; distance= left_d}
    , Dependent {directions= right_dirs; distance= right_d} ) ->
      let distance =
        if Option.equal Int.equal left_d right_d then left_d else None in
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

(** An [Increment] both reads and writes. *)
let reads = function Read | Increment -> true | Write -> false

let writes = function Write | Increment -> true | Read -> false

let accesses_to (var : string) ~keep (accesses : access list) : access list =
  List.filter accesses ~f:(fun access ->
      String.equal access.var var && keep access.kind)

(* ---- Reaching definitions, pruned by subscript ---- *)

(** Find all of the reaching definitions of a variable in an RD set *)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : string) :
    label Set.Poly.t =
  Set.Poly.map
    (Set.Poly.filter rds ~f:(fun (defined, _) -> String.equal defined var))
    ~f:snd

(** [dep] restricted to the directions under which the access at [src] executes
    before the access at [dst] (Kennedy and Allen 2001, Definition 2.1): an
    earlier iteration always does, the same iteration only when [src] is
    lexically first ([src < dst], labels being pre-order). Valid only when no
    loop encloses the shared innermost loop. *)
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

(** The analysed statement, where [mir_reaching_definitions] records definitions
    from outside; the root writes nothing a node reads. *)
let root_label : label = 1

(** The join of [access_dependence] over every source-sink pair, two
    [Increment]s skipped (they commute, design §7.4); [confused] when a side has
    no access, or the nodes' innermost loops differ (directions compare
    iterations of one loop). *)
let pair_dependence ~same_loop (sources : access list) (sinks : access list) :
    dependence =
  if List.is_empty sources || List.is_empty sinks then confused
  else
    let joined =
      List.fold_left sources ~init:Independent ~f:(fun merged source ->
          List.fold_left sinks ~init:merged ~f:(fun merged sink ->
              match (source.kind, sink.kind) with
              | Increment, Increment -> merged
              | _ -> union_dependence merged (access_dependence source sink)))
    in
    match joined with
    | Dependent _ when not same_loop -> confused
    | Independent | Dependent _ -> joined

(** Every label in [sources] whose accesses (selected by [src_accesses]) can
    name an element among [dst_accesses], with the raw dependence of the pair; a
    source outside the statement ([root_label], or not in the map) is
    [confused]. *)
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

(** The definitions of [var] reaching [dst] that may produce an element [dst]
    reads and, when no loop encloses the node's loop, can execute first. *)
let pruned_reaching_defns (statement_map : dep_info_map) (dst : label)
    (var : string) : label Set.Poly.t =
  let _, info = LabelMap.find dst statement_map in
  (* one iteration of an outermost loop is one execution of the body *)
  let ordered =
    Option.value_map info.loop ~default:true ~f:(fun loop ->
        Option.is_none (snd (LabelMap.find loop statement_map)).loop) in
  element_edges statement_map ~dst
    ~sources:(reaching_defn_lookup info.reaching_defn_entry var)
    ~src_accesses:(accesses_to var ~keep:writes)
    ~dst_accesses:(accesses_to var ~keep:reads info.accesses)
  |> List.filter_map ~f:(fun (src, dep) ->
      match if ordered then ordered_dependence ~src ~dst dep else dep with
      | Independent -> None
      | Dependent _ -> Some src)
  |> Set.Poly.of_list

(** Every variable the node reads or increments: right-hand sides, left-hand
    side indices, declaration sizes and initializers, [target()]. *)
let read_variables (info : node_dep_info) : string Set.Poly.t =
  Set.Poly.of_list
    (List.filter_map info.accesses ~f:(fun access ->
         Option.some_if (reads access.kind) access.var))

let node_immediate_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (label : label) :
    label Set.Poly.t =
  let _, info = LabelMap.find label statement_map in
  let rhs_deps =
    Set.Poly.union_map
      (Set.Poly.diff (read_variables info) blockers)
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
        (name, Option.value label_opt ~default:root_label)) in
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
  Set.Poly.union_map labels ~f:(fun label ->
      stmt_rhs_names_set (LabelMap.find label flowgraph_to_mir).pattern)

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
    when negate_right && Expr.Typed.equal left_symbol right_symbol ->
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

(** The [point] of [expr] with respect to [loopvar], read through [+], [-] and
    promotions; any other sub-expression is one symbol. [loopvar] is not in
    [written_vars]. *)
let classify_point ~(loopvar : string option)
    ~(written_vars : string Set.Poly.t) (expr : Expr.Typed.t) : point =
  (* [expr] as one opaque symbol when invariant, else why [expr] varies *)
  let symbolic (expr : Expr.Typed.t) : point =
    let names = expr_var_names_set expr in
    if not (Set.Poly.disjoint names written_vars) then Varying Written
    else if
      Option.value_map loopvar ~default:false ~f:(fun loop_variable ->
          Set.Poly.mem loop_variable names)
    then Varying Nonlinear
    else Invariant {const= 0; symbol= Some expr} in
  let rec classify (expr : Expr.Typed.t) : point =
    let combine ~negate_right lhs rhs =
      match point_combine ~negate_right (classify lhs) (classify rhs) with
      | Some point -> point
      | None -> symbolic expr in
    match expr.pattern with
    | Var name when Option.equal String.equal loopvar (Some name) ->
        Affine {const= 0; symbol= None}
    | Lit (Int, digits) -> (
        match Int.of_string_opt digits with
        | Some const -> Invariant {const; symbol= None}
        | None -> symbolic expr)
    | Promotion (inner, _, _) -> classify inner
    | FunApp (Operator Plus, [lhs; rhs]) -> combine ~negate_right:false lhs rhs
    | FunApp (Operator Minus, [lhs; rhs]) -> combine ~negate_right:true lhs rhs
    | Var _ | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ ->
        symbolic expr in
  classify expr

(** [index] with each index expression replaced by the expression's [point], so
    every Stan index kind ([e], [:], [a:], [a:b], [idxs]) is kept as written. *)
let classify_subscript ~loopvar ~written_vars (index : Expr.Typed.t Index.t) :
    point Index.t =
  Index.map (classify_point ~loopvar ~written_vars) index

(** [Some (name, indices)] when [expr] is a variable under index lists whose
    inner lists are all [Single]: a read [x[i][j]] is [x[i, j]], as [Ast_to_Mir]
    already writes the assignment side; [None] for a slice under another index,
    which [Expr.Helpers.collect_indices] would flatten unsoundly. *)
let rec indexed_variable (expr : Expr.Typed.t) :
    (string * Expr.Typed.t Index.t list) option =
  match expr.pattern with
  | Var name -> Some (name, [])
  | Indexed (base, indices) -> (
      match indexed_variable base with
      | Some (name, prefix)
        when List.for_all prefix ~f:(function
               | Index.Single _ -> true
               | _ -> false) ->
          Some (name, prefix @ indices)
      | Some _ | None -> None)
  | _ -> None

(** Every variable reference inside [expr], in evaluation order; [target()]
    reads ["target"], a [_lp] call increments; the loop variable is omitted. *)
let rec reads_in_expr ~loopvar ~written_vars (expr : Expr.Typed.t) : access list
    =
  let reads_in = reads_in_expr ~loopvar ~written_vars in
  let reads_in_all exprs = List.concat_map exprs ~f:reads_in in
  match expr.pattern with
  | Var name when Option.equal String.equal loopvar (Some name) -> []
  | Var name -> [{var= name; subs= []; kind= Read}]
  | Lit _ -> []
  | Indexed (base, indices) -> (
      match indexed_variable expr with
      | Some (name, all_indices) ->
          let subs =
            List.map all_indices ~f:(classify_subscript ~loopvar ~written_vars)
          in
          {var= name; subs; kind= Read}
          :: reads_in_all (List.concat_map all_indices ~f:Index.bounds)
      | None ->
          reads_in base @ reads_in_all (List.concat_map indices ~f:Index.bounds)
      )
  | FunApp (StanLib (_, FnTarget, _), []) ->
      [{var= "target"; subs= []; kind= Read}]
  | FunApp (UserDefined (_, (FnTarget | FnJacobian)), args) ->
      reads_in_all args @ [{var= "target"; subs= []; kind= Increment}]
  | FunApp (kind, args) -> reads_in_all (Fun_kind.collect_exprs kind @ args)
  | TernaryIf (cond, then_expr, else_expr) ->
      reads_in_all [cond; then_expr; else_expr]
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> reads_in_all [lhs; rhs]
  | Promotion (inner, _, _) | TupleProjection (inner, _) -> reads_in inner

(** The innermost [For] or [While] enclosing [label]. [parents] from
    [build_cf_graphs] holds at most one control-flow node, the nearest [if],
    [while] or [for]; the other members are [break] and [continue] labels. *)
let rec enclosing_loop statement_map parents (label : label) : label option =
  let pattern_of node = fst (LabelMap.find node statement_map) in
  let ctrl_parent =
    List.find_opt
      (Set.Poly.to_list (LabelMap.find label parents))
      ~f:(fun parent -> is_ctrl_flow (pattern_of parent)) in
  Option.bind ctrl_parent ~f:(fun parent ->
      match pattern_of parent with
      | Stmt.Pattern.For _ | While _ -> Some parent
      | _ -> enclosing_loop statement_map parents parent)

(** The accesses of one statement alone, reads before the write; an [if] or a
    loop contributes only the condition or bounds. [target += e] and a [_lp] or
    [_jacobian] call are one [Increment]. *)
let node_accesses ~loopvar ~written_vars
    (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) : access list =
  (* the loop variable is the induction variable here, not a written symbol *)
  let written_vars =
    Option.value_map loopvar ~default:written_vars ~f:(fun loop_variable ->
        Set.Poly.remove loop_variable written_vars) in
  let reads_in = reads_in_expr ~loopvar ~written_vars in
  let reads_in_all exprs = List.concat_map exprs ~f:reads_in in
  let increment_target = {var= "target"; subs= []; kind= Increment} in
  match stmt with
  | Assignment (((lbase, indices) as lhs), _, rhs) ->
      (* a tuple projection is a write of the whole variable *)
      let subs =
        match lbase with
        | LVariable _ ->
            List.map indices ~f:(classify_subscript ~loopvar ~written_vars)
        | LTupleProjection _ -> [] in
      reads_in_all (List.concat_map indices ~f:Index.bounds)
      @ reads_in rhs
      @ [{var= Stmt.Helpers.lhs_variable lhs; subs; kind= Write}]
  | Decl {decl_id; initialize; _} ->
      (match initialize with Assign init -> reads_in init | _ -> [])
      @ [{var= decl_id; subs= []; kind= Write}]
  | TargetPE operand | JacobianPE operand ->
      reads_in operand @ [increment_target]
  | NRFunApp
      ( ( StanLib (_, (FnTarget | FnJacobian), _)
        | UserDefined (_, (FnTarget | FnJacobian)) )
      , args ) ->
      (* the increment [Monotone_framework.assigned_vars_stmt] records too *)
      reads_in_all args @ [increment_target]
  | Return (Some value) -> reads_in value
  | NRFunApp (kind, args) -> reads_in_all (Fun_kind.collect_exprs kind @ args)
  | IfElse (cond, _, _) | While (cond, _) -> reads_in cond
  | For {loopvar= inner; lower; upper; _} ->
      reads_in_all [lower; upper] @ [{var= inner; subs= []; kind= Write}]
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
  (* the loop variable of a [For]; a [While] has none *)
  let loopvar_of loop =
    Option.bind loop ~f:(fun label ->
        match fst (LabelMap.find label statement_map) with
        | Stmt.Pattern.For {loopvar; _} -> Some loopvar
        | _ -> None) in
  LabelMap.mapi statement_map ~f:(fun label (pattern, meta) ->
      let rds = LabelMap.find label rd_map in
      let loop = enclosing_loop statement_map parents label in
      ( pattern
      , { predecessors= LabelMap.find label preds
        ; parents= LabelMap.find label parents
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit
        ; loop
        ; accesses=
            node_accesses ~loopvar:(loopvar_of loop) ~written_vars pattern
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

(** The variables read by the statements at [labels]. *)
let rhs_variables_at (statement_map : dep_info_map) (labels : label Set.Poly.t)
    : string Set.Poly.t =
  Set.Poly.union_map labels ~f:(fun label ->
      read_variables (snd (LabelMap.find label statement_map)))
