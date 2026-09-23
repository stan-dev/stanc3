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

(* The types are documented in the interface. *)

type linear = {const: int; symbol: Expr.Typed.t option; loopvar: string option}
type varying_kind = Written | Nonlinear
type point = Affine of linear | Varying of varying_kind
type access_kind = Read | Write | Increment
type access = {var: string; subs: point Index.t list; kind: access_kind}
type direction = Lt | Eq | Gt
type level = {directions: direction Set.Poly.t; distance: int option}
type dependence = Independent | Dependent of level list

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

(** The loops around both of two accesses, outermost first, each given by the
    loop variable; a [while] loop has no loop variable and is [None]. A
    [dependence] between the two accesses has one [level] per entry. *)
type frame = string option list

(** A loop about which nothing is known: any direction, no known distance. *)
let free = {directions= Set.Poly.of_list [Lt; Eq; Gt]; distance= None}

(** The dependence to assume when two accesses cannot be compared: in every loop
    of [frame], any two iterations may touch the same element. LLVM's dependence
    analysis calls this a confused dependence. *)
let confused (frame : frame) : dependence =
  Dependent (List.map frame ~f:(fun _ -> free))

(** Whether two single indices can be equal (the ZIV and strong SIV tests of
    Goff, Kennedy and Tseng 1991, section 3). Without a loop variable, the two
    indices are equal in every iteration or in none. [n + a] and [n + b], for
    the variable [n] of a loop in [frame], are equal when the two iterations of
    [n] are [a - b] apart. Any other pair is [confused]. *)
let point_dependence (frame : frame) (source : point) (sink : point) :
    dependence =
  match (source, sink) with
  | Affine source_term, Affine sink_term
    when Option.equal String.equal source_term.loopvar sink_term.loopvar -> (
      (* the symbols' values are unknown, so the difference is known only when
         both indices have the same symbol or neither has one *)
      let difference =
        Option.some_if
          (Option.equal Expr.Typed.equal source_term.symbol sink_term.symbol)
          (source_term.const - sink_term.const) in
      match (difference, source_term.loopvar) with
      | None, _ | Some 0, None -> confused frame
      | Some _, None -> Independent
      | Some distance, Some loopvar when List.mem (Some loopvar) ~set:frame ->
          let direction =
            if distance = 0 then Eq else if distance > 0 then Lt else Gt in
          (* only the level of [loopvar] is known *)
          Dependent
            (List.map frame ~f:(fun var ->
                 if Option.equal String.equal var (Some loopvar) then
                   { directions= Set.Poly.singleton direction
                   ; distance= Some distance }
                 else free))
      | Some _, Some _ -> confused frame)
  | Affine _, Affine _ | Varying _, _ | _, Varying _ -> confused frame

(** Whether two accesses to one variable can touch the same element. The
    elements are the same only when the indices are equal at every position, so
    the per-position results are intersected. Accesses with different numbers of
    indices are [confused]. *)
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
        (* keep what is possible at both positions; the accesses are independent
           when nothing is left at some loop *)
        match (merged, position) with
        | Independent, _ | _, Independent -> Independent
        | Dependent merged_levels, Dependent position_levels ->
            let levels =
              List.map2 merged_levels position_levels ~f:(fun left right ->
                  match (left.distance, right.distance) with
                  | Some left_d, Some right_d when left_d <> right_d ->
                      {free with directions= Set.Poly.empty}
                  | _ ->
                      { directions=
                          Set.Poly.inter left.directions right.directions
                      ; distance= Option.first_some left.distance right.distance
                      }) in
            if
              List.exists levels ~f:(fun level ->
                  Set.Poly.is_empty level.directions)
            then Independent
            else Dependent levels)

(** Whether an access of this kind reads the variable; an [Increment] reads and
    writes. *)
let reads = function Read | Increment -> true | Write -> false

let writes = function Write | Increment -> true | Read -> false

(** The accesses to [var] whose kind satisfies [keep]. *)
let accesses_to (var : string) ~keep (accesses : access list) : access list =
  List.filter accesses ~f:(fun access ->
      String.equal access.var var && keep access.kind)

(** Find all of the reaching definitions of a variable in an RD set *)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : string) :
    label Set.Poly.t =
  Set.Poly.map
    (Set.Poly.filter rds ~f:(fun (defined, _) -> String.equal defined var))
    ~f:snd

(** [dep] without the cases where the access at [dst] runs before the access at
    [src] (Kennedy and Allen 2001, definition 2.1). [src] runs first when, at
    the outermost loop whose direction is not [Eq], the direction is [Lt]; or
    when every direction is [Eq] and [src] comes first in the program,
    [src < dst]. [Eq] is kept at a loop only if the loops inside still allow
    [src] to run first. *)
let ordered_dependence ~(src : label) ~(dst : label) (dep : dependence) :
    dependence =
  let rec restrict = function
    | [] -> Option.some_if (src < dst) []
    | level :: inner ->
        let same_iteration =
          if Set.Poly.mem Eq level.directions then restrict inner else None
        in
        let directions =
          Set.Poly.filter level.directions ~f:(function
            | Lt -> true
            | Eq -> Option.is_some same_iteration
            | Gt -> false) in
        if Set.Poly.is_empty directions then None
        else if Set.Poly.mem Lt directions then
          Some ({level with directions} :: inner)
        else Option.map same_iteration ~f:(List.cons {level with directions})
  in
  match dep with
  | Independent -> Independent
  | Dependent levels -> (
      match restrict levels with
      | Some levels -> Dependent levels
      | None -> Independent)

(** The label of the analysed statement. An assignment from before the analysed
    statement is recorded at this label. *)
let root_label : label = 1

(** The dependence from the accesses [sources] to the accesses [sinks]:
    [Independent] only when every pair is independent. [restrict] is applied to
    each pair before the pairs are combined, so that a direction removed for one
    pair is not added back by another. Two [Increment]s are skipped, since
    increments can run in either order. When either list is empty the accesses
    are unknown and the result is [confused]. *)
let pair_dependence (frame : frame) ~(restrict : dependence -> dependence)
    (sources : access list) (sinks : access list) : dependence =
  if List.is_empty sources || List.is_empty sinks then restrict (confused frame)
  else
    List.fold_left sources ~init:Independent ~f:(fun merged source ->
        List.fold_left sinks ~init:merged ~f:(fun merged sink ->
            match (source.kind, sink.kind) with
            | Increment, Increment -> merged
            | _ -> (
                (* a direction possible for either pair is possible; the
                   distance is kept only when both pairs agree *)
                match
                  (merged, restrict (access_dependence frame source sink))
                with
                | Independent, other | other, Independent -> other
                | Dependent merged_levels, Dependent pair_levels ->
                    Dependent
                      (List.map2 merged_levels pair_levels
                         ~f:(fun (left : level) right ->
                           { directions=
                               Set.Poly.union left.directions right.directions
                           ; distance=
                               (if
                                  Option.equal Int.equal left.distance
                                    right.distance
                                then left.distance
                                else None) })))))

(** The [frame] of the statements at [src] and [dst]. *)
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

(** The accesses of the statement at [label]. *)
let accesses_at (statement_map : dep_info_map) (label : label) : access list =
  (snd (LabelMap.find label statement_map)).accesses

(** The labels in [sources] that may touch an element that [dst_accesses] touch,
    each with the dependence. [src_accesses] gives the accesses of a source, and
    [restrict] limits each pair of accesses, for example to the pairs where the
    source runs first. A source outside the analysed statement has unknown
    accesses and is always kept. *)
let element_edges (statement_map : dep_info_map) ~(dst : label)
    ~(sources : label Set.Poly.t)
    ~(restrict : src:label -> dependence -> dependence)
    ~(src_accesses : label -> access list) ~(dst_accesses : access list) :
    (label * dependence) list =
  List.filter_map (Set.Poly.to_list sources) ~f:(fun src ->
      if src = root_label || not (LabelMap.mem src statement_map) then
        Some (src, confused [])
      else
        match
          pair_dependence
            (common_frame statement_map ~src ~dst)
            ~restrict:(restrict ~src) (src_accesses src) dst_accesses
        with
        | Independent -> None
        | Dependent _ as dep -> Some (src, dep))

(** The assignments to [var] that reach [dst] and may write an element that
    [dst] reads before [dst] runs. *)
let pruned_reaching_defns (statement_map : dep_info_map) (dst : label)
    (var : string) : label Set.Poly.t =
  let _, info = LabelMap.find dst statement_map in
  element_edges statement_map ~dst
    ~sources:(reaching_defn_lookup info.reaching_defn_entry var)
    ~restrict:(fun ~src dep -> ordered_dependence ~src ~dst dep)
    ~src_accesses:(fun src ->
      accesses_to var ~keep:writes (accesses_at statement_map src))
    ~dst_accesses:(accesses_to var ~keep:reads info.accesses)
  |> List.map ~f:fst |> Set.Poly.of_list

(** The variables the statement reads, including the variables read inside
    indices and sizes, and the variables the statement increments. *)
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

(** [left + right], or [left - right] when [negate_right]. [None] when the
    result is not a [linear]: two different symbols, two loop variables, or a
    loop variable subtracted. *)
let linear_combine ~negate_right (left : linear) (right : linear) :
    linear option =
  (* [symbol] and [loopvar] each hold at most one term; a term subtracted from
     itself cancels *)
  let combine ~equal left_term right_term =
    match (left_term, right_term) with
    | term, None -> Some term
    | None, Some _ when not negate_right -> Some right_term
    | Some left_value, Some right_value
      when negate_right && equal left_value right_value ->
        Some None
    | None, Some _ | Some _, Some _ -> None in
  let const =
    if negate_right then left.const - right.const else left.const + right.const
  in
  Option.bind (combine ~equal:Expr.Typed.equal left.symbol right.symbol)
    ~f:(fun symbol ->
      Option.map (combine ~equal:String.equal left.loopvar right.loopvar)
        ~f:(fun loopvar -> {const; symbol; loopvar}))

(** The integer index [expr] as a [point]. [+], [-] and promotions are looked
    through; any other expression is one symbol when the expression reads no
    loop variable and no variable in [written_vars]. *)
let classify_point ~(loopvars : string Set.Poly.t)
    ~(written_vars : string Set.Poly.t) (expr : Expr.Typed.t) : point =
  (* an expression the classification does not look inside *)
  let symbolic (expr : Expr.Typed.t) : point =
    let names = expr_var_names_set expr in
    if not (Set.Poly.disjoint names written_vars) then Varying Written
    else if not (Set.Poly.disjoint names loopvars) then Varying Nonlinear
    else Affine {const= 0; symbol= Some expr; loopvar= None} in
  let rec classify (expr : Expr.Typed.t) : point =
    let combine ~negate_right lhs rhs =
      match (classify lhs, classify rhs) with
      | Affine left_term, Affine right_term -> (
          match linear_combine ~negate_right left_term right_term with
          | Some term -> Affine term
          | None -> symbolic expr)
      | Varying _, _ | _, Varying _ -> symbolic expr in
    match expr.pattern with
    | Var name when Set.Poly.mem name loopvars ->
        Affine {const= 0; symbol= None; loopvar= Some name}
    | Lit (Int, digits) -> (
        match Int.of_string_opt digits with
        | Some const -> Affine {const; symbol= None; loopvar= None}
        | None -> symbolic expr)
    | Promotion (inner, _, _) -> classify inner
    | FunApp (Operator Plus, [lhs; rhs]) -> combine ~negate_right:false lhs rhs
    | FunApp (Operator Minus, [lhs; rhs]) -> combine ~negate_right:true lhs rhs
    | Var _ | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ ->
        symbolic expr in
  classify expr

(** The variable and the indices of [expr], when [expr] is a variable with
    indices and every index list except the last holds only single indices.
    [x[i][j]] gives [x] with the indices [i, j], the same form as the left side
    of an assignment. *)
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

(** The variables [expr] reads, in evaluation order. [target()] reads [target],
    and a call to a [_lp] function increments [target]. A loop variable in
    [loopvars] is not counted as a read. *)
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
          (* each index is kept as written: single, [:], [a:], [a:b] or a
             multi-index *)
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

(** The innermost [for] or [while] loop around [label]. [parents] holds, for
    each label, the nearest control-flow statement around the label (and the
    labels of [break] and [continue] statements), so the search walks outward
    one control-flow statement at a time. *)
let rec enclosing_loop statement_map parents (label : label) : label option =
  let pattern_of node = fst (LabelMap.find node statement_map) in
  List.find_opt
    (Set.Poly.to_list (LabelMap.find label parents))
    ~f:(fun parent -> is_ctrl_flow (pattern_of parent))
  |> Option.bind ~f:(fun parent ->
      match pattern_of parent with
      | Stmt.Pattern.For _ | While _ -> Some parent
      | _ -> enclosing_loop statement_map parents parent)

(** The accesses of one statement, reads first and then the write, not counting
    the statements nested inside. An [if] or a loop reads the condition or the
    bounds. A [target +=] statement and a call to a [_lp] function increment
    [target]. *)
let node_accesses ~(loopvars : string Set.Poly.t) ~written_vars
    (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) : access list =
  (* an index that uses a loop variable is classified by the loop variable, so
     the loop variables are removed from [written_vars] *)
  let written_vars = Set.Poly.diff written_vars loopvars in
  let reads_in = reads_in_expr ~loopvars ~written_vars in
  let reads_in_all exprs = List.concat_map exprs ~f:reads_in in
  let increment_target = {var= "target"; subs= []; kind= Increment} in
  match stmt with
  | Assignment (((lbase, indices) as lhs), _, rhs) ->
      (* an assignment to part of a tuple is treated as a write of the whole
         tuple *)
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
  | For {lower; upper; _} -> reads_in_all [lower; upper]
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
  (* the variables assigned anywhere in [stmt]; an index that reads one of them
     can change value inside [stmt] *)
  let written_vars =
    LabelMap.fold statement_map ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:(pattern, _) written ->
        Set.Poly.union written (assigned_or_declared_vars_stmt pattern)) in
  (* the loop variables of the [for] loops around [label] *)
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

(** The variables read by the statements at [labels]. *)
let rhs_variables_at (statement_map : dep_info_map) (labels : label Set.Poly.t)
    : string Set.Poly.t =
  Set.Poly.union_map labels ~f:(fun label ->
      read_variables (snd (LabelMap.find label statement_map)))
