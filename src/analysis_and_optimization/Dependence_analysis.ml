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

(** Dependency analysis is used to check which statements in a block of code are
    affected by which other statements in the block. Pedantic mode uses the
    answer to warn when an [if] depends on a parameter; the factor graph uses
    dependency analysis to find the data and parameters behind each [target]
    term.

    The data structure for dependency analysis is made in four layers, each a
    map keyed by [label], an [int] associated with one MIR statement:
    + {b Statement map} ([Dataflow_utils.build_statement_map]): every statement
      gets a label in pre-order and is stored with the statement's children
      replaced by the children's labels, so the block becomes a flat table.
    + {b Control flow} ([Dataflow_utils.build_cf_graphs]): per label, which
      statements can run before that statement, and the statement's
      {e control parents}, the [if]/[while]/[for] nodes that decide whether the
      statement runs at all.
    + {b Reaching definitions} ([Monotone_framework.reaching_definitions_mfp]):
      per label, pairs [(variable, label')] meaning "the statement at [label']
      may be the last one to have assigned [variable]". Reaching definitions are
      keyed by name: [theta[1] = a] is a definition of [theta], with no index.
    + {b Accesses} (this module): the elements each statement reads and writes,
      indices included, so that a definition of [theta[1]] can be ruled out as a
      source for a read of [theta[2]].

    {2 Running example}

    {[
      parameters { real a; }
      model {
        vector[2] theta;
        theta[1] = a;
        theta[2] = 1;
        if (theta[2] > 0) target += 1;
      }
    ]}

    The statement map of the program's log_prob block. The MIR constructors:
    [SList] is the block's list of statements, [Block] a braced scope, [IfElse]
    an [if], [TargetPE] a [target +=].
    {v
      1  SList [2; 3]
      2    real a;               the parameter, declared by the block
      3    Block [4; 5; 6; 7]    the model block
      4      vector[2] theta;
      5      theta[1] = a;
      6      theta[2] = 1;
      7      if (theta[2] > 0) then 8
      8        Block [9]
      9          target += 1;
    v}

    Node 7 is stored as the pattern [IfElse (theta[2] > 0, 8, None)] together
    with the [node_dep_info] of node 7: predecessors [{6}], parents [{}] (top
    level), reaching definitions on entry
    [{(a, 2); (theta, 4); (theta, 5); (theta, 6)}], accesses [[R theta[2]]].
    Node 9 has parents [{7}] and accesses [[+= target]].

    The dependency graph. A statement depends on the statement's control parents
    and on the definitions of the variables the statement reads, and on the
    dependencies of the control parents and of the defining statements in turn.
    Writing a variable adds no edge; being inside an [if] adds the dependencies
    of the [if].
    {v
      5 -> {2}        reads a, declared at 2
      6 -> {}         reads nothing
      7 -> {4; 6}     reads theta[2]: kept the declaration at 4 (whole variable)
                      and the write at 6; dropped 5, which writes theta[1]
      8 -> {4; 6; 7}  the then-block: guarded by 7, so node 8 inherits the set of 7
      9 -> {4; 6; 7}  likewise
    v}
    So the [if] is not reported as depending on the parameter [a]. *)

(** What the analysis knows about one statement (layers 2 to 4 above). *)
type node_dep_info =
  { predecessors: label Set.Poly.t  (** statements that can run just before *)
  ; parents: label Set.Poly.t
        (** control parents; each control parent is a dependency of the
            statement *)
  ; reaching_defn_entry: reaching_defn Set.Poly.t
        (** [(variable, label)] definitions that may reach this statement *)
  ; reaching_defn_exit: reaching_defn Set.Poly.t
        (** the definitions that may reach the next statement *)
  ; accesses: access list  (** the statement's own indexed reads and writes *)
  ; meta: Location_span.t  (** source location, reported by pedantic mode *) }

(** The block as a flat table: each label's statement, children replaced by the
    children's labels, with the statement's [node_dep_info]. Built by
    [build_dep_info_map]. *)
type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

(** Each label's dependencies, transitively: the statement's control parents and
    the subscript-pruned definitions of the variables the statement reads. *)
type dependency_graph = label Set.Poly.t LabelMap.t

(***********************************)
(* Element test: can two accesses  *)
(* name the same element?          *)
(***********************************)

let confused =
  Dependent {directions= Set.Poly.of_list [Lt; Eq; Gt]; distance= None}

(** [Some (left.const - right.const)] when [left] and [right] carry the same
    symbol, or none; [None] when the symbols differ, so nothing cancels. *)
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
  | Affine _, Invariant _
   |Invariant _, Affine _
   |Varying _, (Affine _ | Invariant _ | Varying _)
   |(Affine _ | Invariant _), Varying _ ->
      confused

(** The dependence at one index position: two [Single] indices are compared; a
    slice or multi-index in [source] or in [sink] gives [confused]. *)
let subscript_dependence (source : point Index.t) (sink : point Index.t) :
    dependence =
  match (source, sink) with
  | Single source_point, Single sink_point ->
      point_dependence source_point sink_point
  | ( (Single _ | All | Upfrom _ | Between _ | MultiIndex _)
    , (All | Upfrom _ | Between _ | MultiIndex _) )
   |(All | Upfrom _ | Between _ | MultiIndex _), Single _ ->
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
        | Some _, None | None, Some _ | None, None -> false in
      if distances_differ || Set.Poly.is_empty directions then Independent
      else
        Dependent
          { directions
          ; distance= Option.first_some merged_distance position_distance }

(** The dependence between [source] and [sink] over every index position;
    [confused] if index counts differ; [Independent] only when never equal. *)
let access_dependence (source : access) (sink : access) : dependence =
  if List.length source.subs <> List.length sink.subs then confused
  else
    List.fold_left2 source.subs sink.subs ~init:confused
      ~f:(fun merged source_sub sink_sub ->
        intersect_dependence merged (subscript_dependence source_sub sink_sub))

(** The accesses in [accesses] that read [var]: kind [Read] or [Increment]. *)
let accesses_reading (var : string) (accesses : access list) : access list =
  List.filter accesses ~f:(fun access ->
      String.equal access.var var
      && match access.kind with Read | Increment -> true | Write -> false)

(** The accesses in [accesses] that write [var]: kind [Write] or [Increment]. *)
let accesses_writing (var : string) (accesses : access list) : access list =
  List.filter accesses ~f:(fun access ->
      String.equal access.var var
      && match access.kind with Write | Increment -> true | Read -> false)

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

(** The labels defining [var] that reach node [info], minus each defining label
    at which every write of [var] is [Independent] of every read of [var] in
    [info]. *)
let pruned_reaching_defns (statement_map : dep_info_map) (info : node_dep_info)
    (var : string) : label Set.Poly.t =
  let defining_labels = reaching_defn_lookup info.reaching_defn_entry var in
  let reads_of_var = accesses_reading var info.accesses in
  (* a definition is kept unless the subscripts rule out every read *)
  let may_define_read_element defining_label =
    match LabelMap.find_opt defining_label statement_map with
    | None -> true
    | Some (_, defining_info) -> (
        match (accesses_writing var defining_info.accesses, reads_of_var) with
        | [], _ | _, [] -> true
        | writes, reads ->
            List.exists writes ~f:(fun write ->
                List.exists reads ~f:(fun read ->
                    match access_dependence write read with
                    | Independent -> false
                    | Dependent _ -> true))) in
  Set.Poly.filter defining_labels ~f:may_define_read_element

let node_immediate_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (label : label) :
    label Set.Poly.t =
  let stmt, info = LabelMap.find label statement_map in
  let rhs_set = Set.Poly.map (stmt_rhs_var_set stmt) ~f:fst in
  let rhs_deps =
    Set.Poly.union_map
      (Set.Poly.diff rhs_set blockers)
      ~f:(pruned_reaching_defns statement_map info) in
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
      ~f:(pruned_reaching_defns statement_map info) in
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

(** {2 Classifying subscripts} *)

(** [left + right], or [left - right] with [~negate_right]; [None] when the
    result would need two symbols or a negated symbol, which [linear] cannot
    hold. *)
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

(** [left + right], or [left - right] with [~negate_right], when the result is
    again [loopvar + offset] or [offset]; [None] otherwise, [Varying] included.
*)
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

(** Whether [name] is the loop variable; [loopvar] is [Some variable] inside a
    [For] and [None] for a statement outside any loop. *)
let is_loopvar ~(loopvar : string option) name =
  match loopvar with
  | Some loop_variable -> String.equal loop_variable name
  | None -> false

(** Whether [expr] mentions at least one variable in [names]. *)
let mentions (names : string Set.Poly.t) (expr : Expr.Typed.t) =
  not (Set.Poly.disjoint (expr_var_names_set expr) names)

(** The loop variable sits under another index somewhere in [expr], as in
    [idx[n]]. *)
let is_gather ~loopvar (expr : Expr.Typed.t) =
  match loopvar with
  | None -> false
  | Some loop_variable ->
      let rec under_index (expr : Expr.Typed.t) =
        match expr.pattern with
        | Indexed (_, indices) ->
            List.exists
              (List.concat_map indices ~f:Index.bounds)
              ~f:(mentions (Set.Poly.singleton loop_variable))
        | Var _ | Lit _ -> false
        | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _
         |TupleProjection _ ->
            Expr.Pattern.fold
              (fun found subexpr -> found || under_index subexpr)
              false expr.pattern in
      under_index expr

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
      else if not (mentions_loopvar expr) then
        Invariant {const= 0; symbol= Some expr}
      else if is_gather ~loopvar expr then Varying Gather
      else Varying Nonlinear in
    let combine ~negate_right lhs rhs =
      match point_combine ~negate_right (classify lhs) (classify rhs) with
      | Some point -> point
      | None -> symbolic () in
    match expr.pattern with
    | Var name when is_loopvar ~loopvar name -> Affine {const= 0; symbol= None}
    | Lit (Int, digits) -> (
        match Int.of_string_opt digits with
        | Some const -> Invariant {const; symbol= None}
        | None -> symbolic ())
    | Promotion (inner, _, _) -> classify inner
    | FunApp (StanLib (name, FnPlain, _), [lhs; rhs]) -> (
        (* built-in operators are [StanLib] calls named by
           [Operator.to_string] *)
        match Operator.of_string_opt name with
        | Some Plus -> combine ~negate_right:false lhs rhs
        | Some Minus -> combine ~negate_right:true lhs rhs
        | Some
            ( Times | Divide | IntDivide | Modulo | LDivide | EltTimes
            | EltDivide | Pow | EltPow | Or | And | Equals | NEquals | Less
            | Leq | Greater | Geq | PNot | PPlus | PMinus | Transpose )
         |None ->
            symbolic ())
    | Var _ | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ ->
        symbolic () in
  classify expr

(** [index] with each index expression replaced by the expression's [point], so
    every Stan index kind ([e], [:], [a:], [a:b], [idxs]) is kept as written. *)
let classify_subscript ~loopvar ~written_vars (index : Expr.Typed.t Index.t) :
    point Index.t =
  Index.map (classify_point ~loopvar ~written_vars) index

(** {2 Accesses} *)

(** Every variable reference inside [expr], in evaluation order; [target()]
    counts as a read of ["target"]; reads of the loop variable are omitted. *)
let rec reads_in_expr ~loopvar ~written_vars ~label (expr : Expr.Typed.t) :
    access list =
  let reads_in = reads_in_expr ~loopvar ~written_vars ~label in
  let reads_in_all = reads_in_exprs ~loopvar ~written_vars ~label in
  match expr.pattern with
  | Var name when is_loopvar ~loopvar name -> []
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

(** The reads inside each expression of [exprs], in order. *)
and reads_in_exprs ~loopvar ~written_vars ~label (exprs : Expr.Typed.t list) :
    access list =
  List.concat_map exprs ~f:(reads_in_expr ~loopvar ~written_vars ~label)

(** The names assigned or declared anywhere inside [stmt]:
    [Monotone_framework.assigned_or_declared_vars_stmt] on each substatement. *)
let rec written_variables (stmt : Stmt.Located.t) : string Set.Poly.t =
  Stmt.Pattern.fold
    (fun written _ -> written)
    (fun written substmt -> Set.Poly.union written (written_variables substmt))
    (assigned_or_declared_vars_stmt stmt.pattern)
    stmt.pattern

(** The loop variable of the innermost [For] enclosing [label], found by
    climbing the control-flow [parents] of [build_cf_graphs] from [label]. *)
let rec enclosing_loopvar
    (statement_map : ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t)
    (parents : label Set.Poly.t LabelMap.t) (label : label) : string option =
  let pattern_of (node_label : int) =
    fst (LabelMap.find node_label statement_map) in
  let ctrl_parent =
    List.find_map
      (Set.Poly.to_list (LabelMap.find label parents))
      ~f:(fun parent ->
        Option.some_if (is_ctrl_flow (pattern_of parent)) parent) in
  match ctrl_parent with
  | None -> None
  | Some parent -> (
      match pattern_of parent with
      | For {loopvar; _} -> Some loopvar
      | IfElse _ | While _ | Assignment _ | TargetPE _ | JacobianPE _
       |NRFunApp _ | Break | Continue | Return _ | Skip | Profile _ | Block _
       |SList _ | Decl _ ->
          enclosing_loopvar statement_map parents parent)

(** The accesses of the statement at [label] alone, reads before the write;
    substatements are not visited, so an [if] or a loop contributes only the
    condition or bounds. [target += e] is one [Increment]. *)
let node_accesses ~loopvar ~written_vars ~label
    (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) : access list =
  let reads_in = reads_in_expr ~loopvar ~written_vars ~label in
  let reads_in_all = reads_in_exprs ~loopvar ~written_vars ~label in
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
  let written_vars = written_variables stmt in
  let accesses : access list LabelMap.t =
    LabelMap.mapi statement_map ~f:(fun label (pattern, _) ->
        node_accesses
          ~loopvar:(enclosing_loopvar statement_map parents label)
          ~written_vars ~label pattern) in
  LabelMap.mapi statement_map ~f:(fun label (stmt, meta) ->
      let rds = LabelMap.find label rd_map in
      ( stmt
      , { predecessors= LabelMap.find label preds
        ; parents= LabelMap.find label parents
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit
        ; accesses= LabelMap.find label accesses
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
