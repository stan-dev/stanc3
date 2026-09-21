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
    + {b Control flow} ([Dataflow_utils.build_cf_graphs]): predecessors, and the
      {e control parents} ([if], [while], [for]) deciding whether a node runs;
    + {b Reaching definitions} ([Monotone_framework.reaching_definitions_mfp]):
      pairs [(variable, label')], "[label'] may be the last to have assigned
      [variable]", keyed by name only;
    + {b Accesses} (this module): the elements each statement reads and writes,
      indices included, so a definition of [theta[1]] is ruled out as a source
      for a read of [theta[2]].

    Running example, labels in the margin:
    {[
      theta[1] = a;          // 5
      theta[2] = 1;          // 6
      if (theta[2] > 0) ...  // 7
    ]}
    The definitions of [theta] reaching node 7 are the declaration and nodes 5
    and 6; the element test drops node 5, so the [if] does not depend on [a]. *)

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

(** The loops enclosing both accesses, outermost first; [None] is a [While]. *)
type frame = string option list

let free = {directions= Set.Poly.of_list [Lt; Eq; Gt]; distance= None}

(** Unconstrained at every level: any two iterations may name one element. *)
let confused (frame : frame) : dependence =
  Dependent (List.map frame ~f:(fun _ -> free))

(** [level] at the level of [loopvar], every other level of [frame] free. *)
let at_level (frame : frame) (loopvar : string) (level : level) : dependence =
  Dependent
    (List.map frame ~f:(fun var ->
         if Option.equal String.equal var (Some loopvar) then level else free))

(** The constant difference when both carry the same symbol or none. *)
let linear_difference (left : linear) (right : linear) : int option =
  Option.some_if
    (Option.equal Expr.Typed.equal left.symbol right.symbol)
    (left.const - right.const)

(** The dependence between two single index expressions over [frame]; a variable
    of a loop not enclosing both ranges over many elements. *)
let point_dependence (frame : frame) (source : point) (sink : point) :
    dependence =
  match (source, sink) with
  | Affine source_term, Affine sink_term
    when String.equal source_term.loopvar sink_term.loopvar
         && List.mem (Some source_term.loopvar) ~set:frame -> (
      (* strong SIV (Goff, Kennedy and Tseng 1991 §3); shared symbols cancel *)
      match linear_difference source_term.offset sink_term.offset with
      | Some distance ->
          let direction =
            if distance = 0 then Eq else if distance > 0 then Lt else Gt in
          at_level frame source_term.loopvar
            {directions= Set.Poly.singleton direction; distance= Some distance}
      | None -> confused frame)
  | Invariant source_offset, Invariant sink_offset -> (
      (* ZIV: with the same symbol the elements differ iff the constants do *)
      match linear_difference source_offset sink_offset with
      | Some 0 | None -> confused frame
      | Some _ -> Independent)
  | Affine _, _ | _, Affine _ | Varying _, _ | _, Varying _ -> confused frame

(** The dependence at both [merged] and [position]: [Independent] if either is,
    or if at some level no direction or no distance suits both. *)
let intersect_dependence (merged : dependence) (position : dependence) :
    dependence =
  match (merged, position) with
  | Independent, _ | _, Independent -> Independent
  | Dependent merged_levels, Dependent position_levels ->
      let levels =
        List.map2 merged_levels position_levels ~f:(fun left right ->
            match (left.distance, right.distance) with
            | Some left_d, Some right_d when left_d <> right_d ->
                {free with directions= Set.Poly.empty}
            | _ ->
                { directions= Set.Poly.inter left.directions right.directions
                ; distance= Option.first_some left.distance right.distance })
      in
      if List.exists levels ~f:(fun level -> Set.Poly.is_empty level.directions)
      then Independent
      else Dependent levels

(** Either may hold: directions unioned per level, distance kept when equal. *)
let union_dependence (left : dependence) (right : dependence) : dependence =
  let union_level (left : level) (right : level) : level =
    { directions= Set.Poly.union left.directions right.directions
    ; distance=
        (if Option.equal Int.equal left.distance right.distance then
           left.distance
         else None) } in
  match (left, right) with
  | Independent, other | other, Independent -> other
  | Dependent left_levels, Dependent right_levels ->
      Dependent (List.map2 left_levels right_levels ~f:union_level)

(** [source] against [sink] per index position; [confused] if counts differ. *)
let access_dependence (frame : frame) (source : access) (sink : access) :
    dependence =
  if List.length source.subs <> List.length sink.subs then confused frame
  else
    List.fold_left2 source.subs sink.subs ~init:(confused frame)
      ~f:(fun merged source_sub sink_sub ->
        let position =
          match (source_sub, sink_sub) with
          | Index.Single source_point, Index.Single sink_point ->
              point_dependence frame source_point sink_point
          | _ -> confused frame in
        intersect_dependence merged position)

(** An [Increment] both reads and writes. *)
let reads = function Read | Increment -> true | Write -> false

let writes = function Write | Increment -> true | Read -> false

let accesses_to (var : string) ~keep (accesses : access list) : access list =
  List.filter accesses ~f:(fun access ->
      String.equal access.var var && keep access.kind)

(** Find all of the reaching definitions of a variable in an RD set *)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : string) :
    label Set.Poly.t =
  Set.Poly.map
    (Set.Poly.filter rds ~f:(fun (defined, _) -> String.equal defined var))
    ~f:snd

(** [dep] restricted to the direction vectors where the access at [src] executes
    before the access at [dst] (Kennedy and Allen 2001, Definition 2.1): the
    outermost non-[Eq] level is [Lt], or all are [Eq] and [src < dst]. *)
let ordered_dependence ~(src : label) ~(dst : label) (dep : dependence) :
    dependence =
  let rec restrict = function
    | [] -> Option.some_if (src < dst) []
    | level :: inner ->
        let directions = Set.Poly.remove Gt level.directions in
        if Set.Poly.is_empty directions then None
        else if Set.Poly.mem Lt directions then
          Some ({level with directions} :: inner)
        else Option.map (restrict inner) ~f:(List.cons {level with directions})
  in
  match dep with
  | Independent -> Independent
  | Dependent levels -> (
      match restrict levels with
      | Some levels -> Dependent levels
      | None -> Independent)

(** The analysed statement, where definitions from outside are recorded. *)
let root_label : label = 1

(** The join of [access_dependence] over every source-sink pair, two
    [Increment]s skipped (they commute); [confused] when a side has no access.
*)
let pair_dependence (frame : frame) (sources : access list)
    (sinks : access list) : dependence =
  if List.is_empty sources || List.is_empty sinks then confused frame
  else
    List.fold_left sources ~init:Independent ~f:(fun merged source ->
        List.fold_left sinks ~init:merged ~f:(fun merged sink ->
            match (source.kind, sink.kind) with
            | Increment, Increment -> merged
            | _ -> union_dependence merged (access_dependence frame source sink)))

(** The [frame] of the loops enclosing both [src] and [dst]. *)
let common_frame (statement_map : dep_info_map) ~(src : label) ~(dst : label) :
    frame =
  let rec loops label =
    match (snd (LabelMap.find label statement_map)).loop with
    | None -> []
    | Some loop -> loop :: loops loop in
  let dst_loops = loops dst in
  List.filter_map
    (List.rev (loops src))
    ~f:(fun loop ->
      Option.some_if
        (List.mem loop ~set:dst_loops)
        (match fst (LabelMap.find loop statement_map) with
        | Stmt.Pattern.For {loopvar; _} -> Some loopvar
        | _ -> None))

(** Every label in [sources] whose accesses (selected by [src_accesses]) can
    name an element among [dst_accesses], with the raw dependence; a source
    outside the statement is kept. *)
let element_edges (statement_map : dep_info_map) ~(dst : label)
    ~(sources : label Set.Poly.t) ~(src_accesses : access list -> access list)
    ~(dst_accesses : access list) : (label * dependence) list =
  List.filter_map (Set.Poly.to_list sources) ~f:(fun src ->
      match LabelMap.find_opt src statement_map with
      | Some (_, src_info) when src <> root_label -> (
          match
            pair_dependence
              (common_frame statement_map ~src ~dst)
              (src_accesses src_info.accesses)
              dst_accesses
          with
          | Independent -> None
          | Dependent _ as dep -> Some (src, dep))
      | Some _ | None -> Some (src, confused []))

(** The definitions of [var] reaching [dst] that may produce an element [dst]
    reads and can execute first. *)
let pruned_reaching_defns (statement_map : dep_info_map) (dst : label)
    (var : string) : label Set.Poly.t =
  let _, info = LabelMap.find dst statement_map in
  element_edges statement_map ~dst
    ~sources:(reaching_defn_lookup info.reaching_defn_entry var)
    ~src_accesses:(accesses_to var ~keep:writes)
    ~dst_accesses:(accesses_to var ~keep:reads info.accesses)
  |> List.filter_map ~f:(fun (src, dep) ->
      match ordered_dependence ~src ~dst dep with
      | Independent -> None
      | Dependent _ -> Some src)
  |> Set.Poly.of_list

(** Every variable the node reads or increments, index and size reads included.
*)
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
  | Affine {loopvar; offset= left_offset}, Invariant right_offset ->
      combined (fun offset -> Affine {loopvar; offset}) left_offset right_offset
  | Invariant left_offset, Affine {loopvar; offset= right_offset}
    when not negate_right ->
      combined (fun offset -> Affine {loopvar; offset}) left_offset right_offset
  | Affine left_term, Affine right_term
    when negate_right && String.equal left_term.loopvar right_term.loopvar ->
      (* [(n + o1) - (n + o2)]: the loop variable cancels *)
      combined
        (fun offset -> Invariant offset)
        left_term.offset right_term.offset
  | Invariant _, Affine _ | Affine _, Affine _ | Varying _, _ | _, Varying _ ->
      None

(** The [point] of [expr] over the enclosing [loopvars], read through [+], [-]
    and promotions; any other sub-expression is one symbol. *)
let classify_point ~(loopvars : string Set.Poly.t)
    ~(written_vars : string Set.Poly.t) (expr : Expr.Typed.t) : point =
  (* [expr] as one opaque symbol when invariant, else why [expr] varies *)
  let symbolic (expr : Expr.Typed.t) : point =
    let names = expr_var_names_set expr in
    if not (Set.Poly.disjoint names written_vars) then Varying Written
    else if not (Set.Poly.disjoint names loopvars) then Varying Nonlinear
    else Invariant {const= 0; symbol= Some expr} in
  let rec classify (expr : Expr.Typed.t) : point =
    let combine ~negate_right lhs rhs =
      match point_combine ~negate_right (classify lhs) (classify rhs) with
      | Some point -> point
      | None -> symbolic expr in
    match expr.pattern with
    | Var name when Set.Poly.mem name loopvars ->
        Affine {loopvar= name; offset= {const= 0; symbol= None}}
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

(** [Some (name, indices)] when [expr] is a variable under all-[Single] index
    lists, so a read [x[i][j]] is [x[i, j]] like the assignment side; else
    [None]. *)
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
    reads ["target"], a [_lp] call increments; loop variables are omitted. *)
let rec reads_in_expr ~loopvars ~written_vars (expr : Expr.Typed.t) :
    access list =
  let reads_in = reads_in_expr ~loopvars ~written_vars in
  let reads_in_all exprs = List.concat_map exprs ~f:reads_in in
  match expr.pattern with
  | Var name when Set.Poly.mem name loopvars -> []
  | Var name -> [{var= name; subs= []; kind= Read}]
  | Lit _ -> []
  | Indexed (base, indices) -> (
      match indexed_variable expr with
      | Some (name, all_indices) ->
          (* every index kind ([e], [:], [a:b], [idxs]) is kept as written *)
          let subs =
            List.map all_indices
              ~f:(Index.map (classify_point ~loopvars ~written_vars)) in
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

(** The innermost [For] or [While] enclosing [label]; [parents] holds at most
    one control-flow node, the nearest, plus [break] and [continue] labels. *)
let rec enclosing_loop statement_map parents (label : label) : label option =
  let pattern_of node = fst (LabelMap.find node statement_map) in
  List.find_opt
    (Set.Poly.to_list (LabelMap.find label parents))
    ~f:(fun parent -> is_ctrl_flow (pattern_of parent))
  |> Option.bind ~f:(fun parent ->
      match pattern_of parent with
      | Stmt.Pattern.For _ | While _ -> Some parent
      | _ -> enclosing_loop statement_map parents parent)

(** The accesses of one statement alone, reads before the write; an [if] or a
    loop contributes only the condition or bounds; [_lp] calls increment. *)
let node_accesses ~loopvars ~written_vars
    (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) : access list =
  (* the loop variables are induction variables here, not written symbols *)
  let written_vars = Set.Poly.diff written_vars loopvars in
  let reads_in = reads_in_expr ~loopvars ~written_vars in
  let reads_in_all exprs = List.concat_map exprs ~f:reads_in in
  let increment_target = {var= "target"; subs= []; kind= Increment} in
  match stmt with
  | Assignment (((lbase, indices) as lhs), _, rhs) ->
      (* a tuple projection is a write of the whole variable *)
      let subs =
        match lbase with
        | LVariable _ ->
            List.map indices
              ~f:(Index.map (classify_point ~loopvars ~written_vars))
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
  (* the variables of the enclosing [For]s; a [While] adds none *)
  let rec loopvars_of label =
    match enclosing_loop statement_map parents label with
    | None -> Set.Poly.empty
    | Some loop -> (
        let outer = loopvars_of loop in
        match fst (LabelMap.find loop statement_map) with
        | Stmt.Pattern.For {loopvar; _} -> Set.Poly.add loopvar outer
        | _ -> outer) in
  LabelMap.mapi statement_map ~f:(fun label (pattern, meta) ->
      let rds = LabelMap.find label rd_map in
      ( pattern
      , { predecessors= LabelMap.find label preds
        ; parents= LabelMap.find label parents
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit
        ; loop= enclosing_loop statement_map parents label
        ; accesses=
            node_accesses ~loopvars:(loopvars_of label) ~written_vars pattern
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
