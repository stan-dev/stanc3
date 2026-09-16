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
(* Access model: the elements a    *)
(* node reads and writes           *)
(***********************************)

(** {2 Linear forms} *)

(** [Some k] when [expr] is the integer literal [k], negative literals included.
*)
let int_literal (expr : Expr.Typed.t) =
  match expr.pattern with
  | Lit (Int, digits) -> Int.of_string_opt digits
  | Lit _ | Var _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
   |Promotion _ | TupleProjection _ ->
      None

let linear_const const : linear = {const; terms= []}

(** The linear form [0 + 1 * term], keeping [term] as one opaque symbol. *)
let linear_symbol term : linear = {const= 0; terms= [(1, term)]}

(** Merge equal terms, drop zero coefficients, sort. *)
let linear_normalize ({const; terms} : linear) : linear =
  (* [merged] holds the merged terms seen so far, most recent first *)
  let merge_into merged (coeff, term) =
    match merged with
    | (prev_coeff, prev_term) :: rest when Expr.Typed.compare term prev_term = 0
      ->
        (coeff + prev_coeff, prev_term) :: rest
    | [] | (_, _) :: _ -> (coeff, term) :: merged in
  let terms =
    List.sort terms ~cmp:(fun (_, term1) (_, term2) ->
        Expr.Typed.compare term1 term2)
    |> List.fold_left ~init:[] ~f:merge_into
    |> List.filter ~f:(fun (coeff, _) -> coeff <> 0)
    |> List.rev in
  {const; terms}

let linear_scale factor ({const; terms} : linear) : linear =
  linear_normalize
    { const= factor * const
    ; terms= List.map terms ~f:(fun (coeff, term) -> (factor * coeff, term)) }

let linear_add (left : linear) (right : linear) : linear =
  linear_normalize
    {const= left.const + right.const; terms= left.terms @ right.terms}

let linear_sub left right = linear_add left (linear_scale (-1) right)

(** [Some (op, args)] when [expr] applies a built-in operator; the MIR stores
    built-in operators as [StanLib] calls named by [Operator.to_string]. *)
let operator_app (expr : Expr.Typed.t) : (Operator.t * Expr.Typed.t list) option
    =
  match expr.pattern with
  | FunApp (StanLib (name, FnPlain, _), args) ->
      Option.map (Operator.of_string_opt name) ~f:(fun op -> (op, args))
  | FunApp (_, _)
   |Var _ | Lit _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _ | Promotion _
   |TupleProjection _ ->
      None

(** Whether [name] is the loop variable; [loopvar] is [Some variable] inside a
    [For] and [None] for a statement outside any loop. *)
let is_loopvar ~(loopvar : string option) name =
  match loopvar with
  | Some loop_variable -> String.equal loop_variable name
  | None -> false

(** [Some (coeff, offset)] when [expr = coeff * loopvar + offset], else [None];
    reads [+], [-], literal [*]; [invariant] sub-expressions become symbols. *)
let rec linear_form ~(loopvar : string option)
    ~(invariant : ExprSet.elt -> bool) (expr : Expr.Typed.t) :
    (int * linear) option =
  let open Option.Syntax in
  let recur = linear_form ~loopvar ~invariant in
  (* [expr] as one opaque symbolic term, if [expr] is invariant at all *)
  let symbolic () =
    if invariant expr then Some (0, linear_symbol expr) else None in
  match expr.pattern with
  | Var name when is_loopvar ~loopvar name -> Some (1, linear_const 0)
  | Lit (Int, digits) -> (
      match Int.of_string_opt digits with
      | Some value -> Some (0, linear_const value)
      | None -> symbolic ())
  | Promotion (inner, _, _) -> recur inner
  | FunApp _ -> (
      match operator_app expr with
      | Some (((Plus | Minus) as op), [lhs; rhs]) -> (
          let* lhs_coeff, lhs_offset = recur lhs in
          let+ rhs_coeff, rhs_offset = recur rhs in
          match op with
          | Plus -> (lhs_coeff + rhs_coeff, linear_add lhs_offset rhs_offset)
          | Minus -> (lhs_coeff - rhs_coeff, linear_sub lhs_offset rhs_offset)
          | _ -> assert false
          (* or list the other constructors *))
      | Some (Times, [lhs; rhs]) -> (
          match (int_literal lhs, int_literal rhs) with
          | Some factor, _ ->
              let+ coeff, offset = recur rhs in
              (factor * coeff, linear_scale factor offset)
          | None, Some factor ->
              let+ coeff, offset = recur lhs in
              (factor * coeff, linear_scale factor offset)
          | None, None -> symbolic ())
      | Some (PPlus, [operand]) -> recur operand
      | Some (PMinus, [operand]) ->
          let+ coeff, offset = recur operand in
          (-coeff, linear_scale (-1) offset)
      | Some ((Plus | Minus | PPlus | PMinus | Times), _)
       |Some
          ( ( Divide | IntDivide | Modulo | LDivide | EltTimes | EltDivide | Pow
            | EltPow | Or | And | Equals | NEquals | Less | Leq | Greater | Geq
            | PNot | Transpose )
          , _ )
       |None ->
          symbolic ())
  | Var _
   |Lit ((Real | Imaginary | Str), _)
   |TernaryIf _ | EAnd _ | EOr _ | Indexed _ | TupleProjection _ ->
      symbolic ()

(** {2 Classifying subscripts} *)

(** The names assigned or declared anywhere inside [stmt]:
    [Monotone_framework.assigned_or_declared_vars_stmt] on each substatement. *)
let rec written_variables (stmt : Stmt.Located.t) : string Set.Poly.t =
  Stmt.Pattern.fold
    (fun written _ -> written)
    (fun written substmt -> Set.Poly.union written (written_variables substmt))
    (assigned_or_declared_vars_stmt stmt.pattern)
    stmt.pattern

(** Whether [expr] mentions at least one variable in [names]. *)
let mentions names (expr : Expr.Typed.t) =
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

(** The [point] of [expr] with respect to [loopvar]; a name from [written_vars]
    other than the loop variable makes [expr] [Varying Written]. *)
let classify_point ~loopvar ~written_vars (expr : Expr.Typed.t) : point =
  let written_vars =
    Option.value_map loopvar ~default:written_vars ~f:(fun loop_variable ->
        Set.Poly.remove loop_variable written_vars) in
  let mentions_loopvar expr =
    Option.value_map loopvar ~default:false ~f:(fun loop_variable ->
        mentions (Set.Poly.singleton loop_variable) expr) in
  let invariant expr =
    not (mentions_loopvar expr || mentions written_vars expr) in
  match linear_form ~loopvar ~invariant expr with
  | Some (0, offset) -> Invariant offset
  | Some (coeff, offset) -> Affine {coeff; offset}
  | None when mentions written_vars expr -> Varying Written
  | None when is_gather ~loopvar expr -> Varying Gather
  | None -> Varying Nonlinear

(** [index] with each index expression replaced by the expression's [point], so
    every Stan index kind ([e], [:], [a:], [a:b], [idxs]) is kept as written. *)
let classify_subscript ~loopvar ~written_vars (index : Expr.Typed.t Index.t) :
    point Index.t =
  Index.map (classify_point ~loopvar ~written_vars) index

(** {2 Accesses} *)

let read ~label var subs = {var; subs; kind= Read; label}
let write ~label var subs = {var; subs; kind= Write; label}
let increment ~label var = {var; subs= []; kind= Increment; label}
let index_bounds indices = List.concat_map indices ~f:Index.bounds

(** Every variable reference inside [expr], in evaluation order; [target()]
    counts as a read of ["target"]; reads of the loop variable are omitted. *)
let rec expr_reads ~loopvar ~written_vars ~label (expr : Expr.Typed.t) :
    access list =
  let reads = expr_reads ~loopvar ~written_vars ~label in
  let reads_all exprs = List.concat_map exprs ~f:reads in
  match expr.pattern with
  | Var name when is_loopvar ~loopvar name -> []
  | Var name -> [read ~label name []]
  | Lit _ -> []
  | Indexed ({pattern= Var name; _}, indices) ->
      let subscripts =
        List.map indices ~f:(classify_subscript ~loopvar ~written_vars) in
      read ~label name subscripts :: reads_all (index_bounds indices)
  | Indexed (base, indices) -> reads base @ reads_all (index_bounds indices)
  | FunApp ((StanLib (_, FnTarget, _) | UserDefined (_, FnTarget)), args) ->
      read ~label "target" [] :: reads_all args
  | FunApp (kind, args) -> reads_all (Fun_kind.collect_exprs kind @ args)
  | TernaryIf (cond, then_expr, else_expr) ->
      reads_all [cond; then_expr; else_expr]
  | EAnd (lhs, rhs) | EOr (lhs, rhs) -> reads_all [lhs; rhs]
  | Promotion (inner, _, _) | TupleProjection (inner, _) -> reads inner

(** The accesses of [stmt] alone, reads before the write; substatements are
    separate nodes and contribute nothing. [target += e] is one [Increment]. *)
let node_accesses_of_pattern ~loopvar ~written_vars ~label
    (stmt : (Expr.Typed.t, label) Stmt.Pattern.t) : access list =
  let reads = expr_reads ~loopvar ~written_vars ~label in
  let reads_all exprs = List.concat_map exprs ~f:reads in
  let write = write ~label in
  match stmt with
  | Assignment ((LVariable name, indices), _, rhs) ->
      let subscripts =
        List.map indices ~f:(classify_subscript ~loopvar ~written_vars) in
      reads_all (index_bounds indices) @ reads rhs @ [write name subscripts]
  | Assignment (((LTupleProjection _, _) as lhs), _, rhs) ->
      reads_all (index_bounds (Stmt.Helpers.lhs_indices lhs))
      @ reads rhs
      @ [write (Stmt.Helpers.lhs_variable lhs) []]
  | Decl {decl_id; initialize= Assign init; _} -> reads init @ [write decl_id []]
  | Decl {decl_id; _} -> [write decl_id []]
  | TargetPE operand | JacobianPE operand ->
      reads operand @ [increment ~label "target"]
  | Return (Some value) -> reads value
  | NRFunApp (kind, args) -> reads_all (Fun_kind.collect_exprs kind @ args)
  | IfElse (cond, _, _) | While (cond, _) -> reads cond
  | For {loopvar= inner; lower; upper; _} ->
      reads_all [lower; upper] @ [write inner []]
  | Profile _ | Block _ | SList _ | Break | Continue | Skip | Return None -> []

(** The accesses that read [var] ([Read] or [Increment]). *)
let reads_of ~var (accesses : access list) =
  List.filter accesses ~f:(fun access ->
      String.equal access.var var && access_reads access)

(** The accesses that write [var] ([Write] or [Increment]). *)
let writes_of ~var (accesses : access list) =
  List.filter accesses ~f:(fun access ->
      String.equal access.var var && access_writes access)

(***********************************)
(* Element test: can two accesses  *)
(* name the same element?          *)
(***********************************)

let all_directions = Set.Poly.of_list [Lt; Eq; Gt]
let confused = Dependent {directions= all_directions; distance= None}

(** The dependence with the single direction given by the sign of [distance]. *)
let dependence_at_distance distance =
  let direction = if distance = 0 then Eq else if distance > 0 then Lt else Gt in
  Dependent {directions= Set.Poly.singleton direction; distance= Some distance}

(** The dependence between two single index expressions. *)
let point_dependence (source : point) (sink : point) : dependence =
  match (source, sink) with
  | ( Affine {coeff= source_coeff; offset= source_offset}
    , Affine {coeff= sink_coeff; offset= sink_offset} )
    when source_coeff = sink_coeff -> (
      (* strong SIV (Goff, Kennedy and Tseng 1991 §3): [c*i1 + o1 = c*i2 + o2]
         iff [i2 - i1 = (o1 - o2) / c]; identical symbols cancel *)
      match linear_sub source_offset sink_offset with
      | {const; terms= []} ->
          if const mod source_coeff <> 0 then Independent
          else dependence_at_distance (const / source_coeff)
      | {terms= _ :: _; _} -> confused)
  | Invariant source_offset, Invariant sink_offset -> (
      (* ZIV: same symbols, so the elements differ iff the constants do *)
      match linear_sub source_offset sink_offset with
      | {const; terms= []} -> if const <> 0 then Independent else confused
      | {terms= _ :: _; _} -> confused)
  | Affine _, Affine _
   |Affine _, Invariant _
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

(** Fold the dependence [position] into [merged]: [Independent] if [merged] or
    [position] is, or the distances differ, or the directions are disjoint. *)
let merge_positions (merged : dependence) (position : dependence) : dependence =
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
        merge_positions merged (subscript_dependence source_sub sink_sub))

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
    at which every write of [var] is [Independent] of every read in [info]. *)
let reaching_defns_of_read (statement_map : dep_info_map) (info : node_dep_info)
    (var : string) : label Set.Poly.t =
  let defs = reaching_defn_lookup info.reaching_defn_entry var in
  let reads = reads_of ~var info.accesses in
  let may_reach def_label =
    match LabelMap.find_opt def_label statement_map with
    | None -> true
    | Some (_, def_info) -> (
        match (writes_of ~var def_info.accesses, reads) with
        | [], _ | _, [] -> true
        | writes, reads ->
            List.exists writes ~f:(fun write ->
                List.exists reads ~f:(fun read ->
                    match access_dependence write read with
                    | Independent -> false
                    | Dependent _ -> true))) in
  Set.Poly.filter defs ~f:may_reach

let node_immediate_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (label : label) :
    label Set.Poly.t =
  let stmt, info = LabelMap.find label statement_map in
  let rhs_set = Set.Poly.map (stmt_rhs_var_set stmt) ~f:fst in
  let rhs_deps =
    Set.Poly.union_map
      (Set.Poly.diff rhs_set blockers)
      ~f:(reaching_defns_of_read statement_map info) in
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
      ~f:(reaching_defns_of_read statement_map info) in
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

(** The loop variable of the innermost [For] enclosing [label], found by
    climbing the control-flow [parents] of [build_cf_graphs] from [label]. *)
let rec enclosing_loopvar
    (statement_map : ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t)
    (parents : label Set.Poly.t LabelMap.t) (label : label) : string option =
  let pattern_of node = fst (LabelMap.find node statement_map) in
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

(** For every label in [statement_map], the accesses of that statement alone,
    classified against the statement's innermost [For] and [written_vars]. *)
let node_accesses_map ~(written_vars : string Set.Poly.t)
    ~(parents : label Set.Poly.t LabelMap.t)
    (statement_map : ((Expr.Typed.t, label) Stmt.Pattern.t * 'm) LabelMap.t) :
    access list LabelMap.t =
  LabelMap.mapi statement_map ~f:(fun label (pattern, _) ->
      node_accesses_of_pattern
        ~loopvar:(enclosing_loopvar statement_map parents label)
        ~written_vars ~label pattern)

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    dep_info_map =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = mir_reaching_definitions mir stmt in
  let accesses =
    node_accesses_map ~written_vars:(written_variables stmt) ~parents
      statement_map in
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

(***********************************)
(* Printers                        *)
(***********************************)

(** Prints [k+1] or [+k-2*m+1]; with [leading] the first item drops the leading
    [+], and a term-free constant is printed even when the constant is [0]. *)
let pp_linear ~leading ppf ({const; terms} : linear) =
  let sign ~first coeff = if coeff < 0 then "-" else if first then "" else "+" in
  List.iteri terms ~f:(fun position (coeff, term) ->
      let prefix = sign ~first:(leading && position = 0) coeff in
      match abs coeff with
      | 1 -> Fmt.pf ppf "%s%a" prefix Expr.Typed.pp term
      | magnitude -> Fmt.pf ppf "%s%d*%a" prefix magnitude Expr.Typed.pp term);
  let no_terms = List.is_empty terms in
  if const <> 0 || (leading && no_terms) then
    Fmt.pf ppf "%s%d" (sign ~first:(leading && no_terms) const) (abs const)

let pp_varying_kind ppf = function
  | Written -> Fmt.string ppf "written"
  | Gather -> Fmt.string ppf "gather"
  | Nonlinear -> Fmt.string ppf "nonlinear"

(** [i], [i+1], [-i+2], [2i+k-1] for [Affine]; [3], [k+1] for [Invariant];
    [?gather], [?written], ... for [Varying]. *)
let pp_point ppf = function
  | Invariant offset -> pp_linear ~leading:true ppf offset
  | Affine {coeff; offset} ->
      (match coeff with
      | 1 -> Fmt.string ppf "i"
      | -1 -> Fmt.string ppf "-i"
      | stride -> Fmt.pf ppf "%di" stride);
      pp_linear ~leading:false ppf offset
  | Varying kind -> Fmt.pf ppf "?%a" pp_varying_kind kind

(** Prints [i+1], [:], [k:], [1:k]; a multi-index is braced as [{idxs}] because
    [Index.pp] prints a multi-index like a single index. *)
let pp_subscript ppf (index : point Index.t) =
  match index with
  | MultiIndex indices -> Fmt.pf ppf "{%a}" pp_point indices
  | All | Single _ | Upfrom _ | Between _ -> Index.pp pp_point ppf index

(** [W v[i+1]], [R v], [+= target]. *)
let pp_access ppf {var; subs; kind; _} =
  Fmt.pf ppf "%s %s"
    (match kind with Write -> "W" | Read -> "R" | Increment -> "+=")
    var;
  if not (List.is_empty subs) then
    Fmt.pf ppf "[%a]" Fmt.(list ~sep:(any ", ") pp_subscript) subs

(** Comma-separated accesses, or [none]. *)
let pp_accesses ppf = function
  | [] -> Fmt.string ppf "none"
  | accesses -> Fmt.(list ~sep:(any ", ") pp_access) ppf accesses

let pp_direction ppf = function
  | Lt -> Fmt.string ppf "<"
  | Eq -> Fmt.string ppf "="
  | Gt -> Fmt.string ppf ">"

(** [independent], or [{<,=,>}] with [d=k] when the distance is known, e.g.
    [{<} d=1], [{=} d=0], [{<,=,>}]. *)
let pp_dependence ppf = function
  | Independent -> Fmt.string ppf "independent"
  | Dependent {directions; distance} ->
      Fmt.pf ppf "{%a}"
        Fmt.(list ~sep:(any ",") pp_direction)
        (Set.Poly.to_list directions);
      Option.iter distance ~f:(Fmt.pf ppf " d=%d")

let pp_labels ppf (labels : label Set.Poly.t) =
  if Set.Poly.is_empty labels then Fmt.string ppf "none"
  else Fmt.(list ~sep:(any " ") int) ppf (Set.Poly.to_list labels)

(** One line per label, [label: dependencies]. *)
let pp_dependency_graph ppf (graph : dependency_graph) =
  LabelMap.iter graph ~f:(fun ~key ~data ->
      Fmt.pf ppf "%d: %a@." key pp_labels data)

(** One line [label: accesses] per label that has accesses; labels without
    accesses (blocks, [break], ...) are left out. *)
let pp_node_accesses ppf (statement_map : dep_info_map) =
  LabelMap.iter statement_map ~f:(fun ~key ~data:(_, info) ->
      match info.accesses with
      | [] -> ()
      | accesses -> Fmt.pf ppf "%d: %a@." key pp_accesses accesses)
