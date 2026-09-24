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
type 'index step = Subscript of 'index Index.t | Field of int
type 'index access = {var: string; path: 'index step list; kind: access_kind}

(** [left + right]; [None] when both have a symbol or both have a loop variable,
    since a [linear] holds at most one of each. *)
let linear_add (left : linear) (right : linear) : linear option =
  let add left_term right_term =
    match (left_term, right_term) with
    | term, None | None, term -> Some term
    | Some _, Some _ -> None in
  Option.bind (add left.symbol right.symbol) ~f:(fun symbol ->
      Option.map (add left.loopvar right.loopvar) ~f:(fun loopvar ->
          {const= left.const + right.const; symbol; loopvar}))

(** [left - right]; [None] when a symbol or loop variable of [right] is not the
    same one in [left], since a [linear] cannot hold a subtracted term. *)
let linear_subtract (left : linear) (right : linear) : linear option =
  (* a term of [right] cancels only the same term of [left] *)
  let subtract ~equal left_term right_term =
    match (left_term, right_term) with
    | term, None -> Some term
    | Some left_value, Some right_value when equal left_value right_value ->
        Some None
    | _, Some _ -> None in
  Option.bind (subtract ~equal:Expr.Typed.equal left.symbol right.symbol)
    ~f:(fun symbol ->
      Option.map (subtract ~equal:String.equal left.loopvar right.loopvar)
        ~f:(fun loopvar -> {const= left.const - right.const; symbol; loopvar}))

(** The integer index [expr] as a [point]. [+], [-] and promotions are looked
    through; any other expression is one symbol when the expression reads no
    loop variable and no variable in [written_vars]. *)
let classify_point ~(loopvars : string Set.Poly.t)
    ~(written_vars : string Set.Poly.t) (expr : Expr.Typed.t) : point =
  (* an expression the classification does not look inside *)
  let symbolic (subexpr : Expr.Typed.t) : point =
    let names = expr_var_names_set subexpr in
    if not (Set.Poly.disjoint names written_vars) then Varying Written
    else if not (Set.Poly.disjoint names loopvars) then Varying Nonlinear
    else Affine {const= 0; symbol= Some subexpr; loopvar= None} in
  let rec classify (subexpr : Expr.Typed.t) : point =
    let combine linear_op lhs rhs =
      match (classify lhs, classify rhs) with
      | Affine left_term, Affine right_term -> (
          match linear_op left_term right_term with
          | Some term -> Affine term
          | None -> symbolic subexpr)
      | Varying _, _ | _, Varying _ -> symbolic subexpr in
    match subexpr.pattern with
    | Var name when Set.Poly.mem name loopvars ->
        Affine {const= 0; symbol= None; loopvar= Some name}
    | Lit (Int, digits) -> (
        match Int.of_string_opt digits with
        | Some const -> Affine {const; symbol= None; loopvar= None}
        | None -> symbolic subexpr)
    | Promotion (inner, _, _) -> classify inner
    | FunApp (Operator Plus, [lhs; rhs]) -> combine linear_add lhs rhs
    | FunApp (Operator Minus, [lhs; rhs]) -> combine linear_subtract lhs rhs
    | Var _ | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ ->
        symbolic subexpr in
  classify expr

(** A path is the [step list] from a variable to the part an access touches. *)
module Path = struct
  (** [path] with every single index classified as a [point]. *)
  let classify ~loopvars ~written_vars (path : Expr.Typed.t step list) :
      point step list =
    List.map path ~f:(function
      | Subscript index ->
          Subscript (Index.map (classify_point ~loopvars ~written_vars) index)
      | Field field -> Field field)

  (** The variable and the path of [expr], when [expr] is a variable with
      indices and tuple fields and every step before the last index list is a
      single index or a field. [x[i][j].2] gives [x] with the path [i, j, .2],
      the same form as the left side of an assignment. *)
  let rec of_expr (expr : Expr.Typed.t) :
      (string * Expr.Typed.t step list) option =
    let extend base steps =
      match of_expr base with
      | Some (name, prefix)
        when List.for_all prefix ~f:(function
               | Subscript (Single _) | Field _ -> true
               | Subscript _ -> false) ->
          Some (name, prefix @ steps)
      | Some _ | None -> None in
    match expr.pattern with
    | Var name -> Some (name, [])
    | Indexed (base, indices) ->
        extend base (List.map indices ~f:(fun index -> Subscript index))
    | TupleProjection (base, field) -> extend base [Field field]
    | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ -> None

  (** The path of the left side of an assignment: [x[i].2[j] = ...] gives
      [i, .2, j]. *)
  let rec of_lvalue ((lbase, indices) : Expr.Typed.t Stmt.Pattern.lvalue) :
      Expr.Typed.t step list =
    (match lbase with
      | LVariable _ -> []
      | LTupleProjection (inner, field) -> of_lvalue inner @ [Field field])
    @ List.map indices ~f:(fun index -> Subscript index)

  (** The expressions inside the indices of [path]. *)
  let bounds (path : Expr.Typed.t step list) : Expr.Typed.t list =
    List.concat_map path ~f:(function
      | Subscript index -> Index.bounds index
      | Field _ -> [])
end

module Accesses = struct
  type 'index t = {reads: 'index access list; writes: 'index access list}

  (** One read of [var] at [path]. *)
  let read (var : string) (path : 'index step list) : 'index t =
    {reads= [{var; path; kind= Read}]; writes= []}

  (** One write of [var] at [path]. *)
  let write (var : string) (path : 'index step list) : 'index t =
    {reads= []; writes= [{var; path; kind= Write}]}

  (** An increment of [target], which reads and writes [target]. *)
  let increment_target : 'index t =
    let increment = {var= "target"; path= []; kind= Increment} in
    {reads= [increment]; writes= [increment]}

  (** The accesses of [parts], one part after another. *)
  let concat (parts : 'index t list) : 'index t =
    { reads= List.concat_map parts ~f:(fun part -> part.reads)
    ; writes= List.concat_map parts ~f:(fun part -> part.writes) }

  (** The accesses in [accesses] to [var]. *)
  let of_var (var : string) (accesses : 'index t) : 'index t =
    let keep = List.filter ~f:(fun access -> String.equal access.var var) in
    {reads= keep accesses.reads; writes= keep accesses.writes}

  (** The accesses of [expr], in evaluation order, with the paths as written.
      [target()] reads [target], and a call to a [_lp] function increments
      [target]. *)
  let rec of_expr (expr : Expr.Typed.t) : Expr.Typed.t t =
    (* the accesses of the expressions directly inside [expr], in order *)
    let of_children () =
      concat
        (List.rev
           (Expr.Pattern.fold
              (fun parts subexpr -> of_expr subexpr :: parts)
              [] expr.pattern)) in
    match expr.pattern with
    | Var name -> read name []
    | Indexed _ | TupleProjection _ -> (
        match Path.of_expr expr with
        | Some (name, path) ->
            (* each index is kept as written: single, [:], [a:], [a:b] or a
               multi-index *)
            concat (read name path :: List.map (Path.bounds path) ~f:of_expr)
        | None -> of_children ())
    | FunApp (StanLib (_, FnTarget, _), []) -> read "target" []
    | FunApp (UserDefined (_, (FnTarget | FnJacobian)), _) ->
        concat [of_children (); increment_target]
    | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
        of_children ()

  (** The reads and the writes of one statement, not counting the statements
      nested inside. A declaration reads the sizes in its type. An [Increment]
      is in both lists. *)
  let of_stmt (stmt : (Expr.Typed.t, 'substatement) Stmt.Pattern.t) :
      Expr.Typed.t t =
    (* the accesses of the expressions of [stmt], in order, skipping the
       statements nested inside *)
    let of_children () =
      concat
        (List.rev
           (Stmt.Pattern.fold
              (fun parts subexpr -> of_expr subexpr :: parts)
              Fun.const [] stmt)) in
    match stmt with
    | Assignment (lhs, _, rhs) ->
        let path = Path.of_lvalue lhs in
        concat
          (List.map (Path.bounds path @ [rhs]) ~f:of_expr
          @ [write (Stmt.Helpers.lhs_variable lhs) path])
    | Decl {decl_id; _} -> concat [of_children (); write decl_id []]
    | TargetPE _ | JacobianPE _
     |NRFunApp
        ( ( StanLib (_, (FnTarget | FnJacobian), _)
          | UserDefined (_, (FnTarget | FnJacobian)) )
        , _ ) ->
        concat [of_children (); increment_target]
    | NRFunApp _ | Return _ | IfElse _ | While _ | For _ | Profile _ | Block _
     |SList _ | Break | Continue | Skip ->
        of_children ()

  (** [accesses] with every path classified. The reads of [loopvars] are
      dropped, since a loop variable is part of an index instead. *)
  let classify ~loopvars ~written_vars (accesses : Expr.Typed.t t) : point t =
    let classify_paths =
      List.map ~f:(fun access ->
          {access with path= Path.classify ~loopvars ~written_vars access.path})
    in
    { reads=
        classify_paths
          (List.filter accesses.reads ~f:(fun access ->
               not (Set.Poly.mem access.var loopvars)))
    ; writes= classify_paths accesses.writes }
end

type direction = Lt | Eq | Gt
type level = {directions: direction Set.Poly.t; distance: int option}
type dependence = Independent | Unknown | Dependent of level list

type node_dep_info =
  { predecessors: label Set.Poly.t
  ; parents: label Set.Poly.t
  ; reaching_defn_entry: reaching_defn Set.Poly.t
  ; reaching_defn_exit: reaching_defn Set.Poly.t
  ; loop: label option
  ; accesses: point Accesses.t
  ; meta: Location_span.t }

type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

type dependency_graph = label Set.Poly.t LabelMap.t

(** The loops around both of two accesses, outermost first, each given by the
    loop variable; a [while] loop has no loop variable and is [None]. A
    [Dependent] result for the two accesses has one [level] per entry. *)
type frame = string option list

(** Whether two single indices can be equal (the ZIV and strong SIV tests of
    Goff, Kennedy and Tseng 1991, section 3). Without a loop variable, the two
    indices are equal in every iteration or in none. [n + a] and [n + b], for
    the variable [n] of a loop in [frame], are equal when the two iterations of
    [n] are [a - b] apart. Any other pair is [Unknown]. *)
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
      | None, _ | Some 0, None -> Unknown
      | Some _, None -> Independent
      | Some distance, Some loopvar when List.mem (Some loopvar) ~set:frame ->
          let direction =
            if distance = 0 then Eq else if distance > 0 then Lt else Gt in
          (* only the level of [loopvar] is known; any direction is possible at
             the other loops *)
          Dependent
            (List.map frame ~f:(fun var ->
                 if Option.equal String.equal var (Some loopvar) then
                   { directions= Set.Poly.singleton direction
                   ; distance= Some distance }
                 else {directions= Set.Poly.of_list [Lt; Eq; Gt]; distance= None}))
      | Some _, Some _ -> Unknown)
  | Affine _, Affine _ | Varying _, _ | _, Varying _ -> Unknown

(** What is possible at both of two index positions; the accesses are
    independent when no direction is left at some loop. *)
let meet (left : dependence) (right : dependence) : dependence =
  match (left, right) with
  | Independent, _ | _, Independent -> Independent
  | Unknown, other | other, Unknown -> other
  | Dependent left_levels, Dependent right_levels ->
      let levels =
        List.map2 left_levels right_levels
          ~f:(fun (left_level : level) right_level ->
            match (left_level.distance, right_level.distance) with
            | Some left_distance, Some right_distance
              when left_distance <> right_distance ->
                {directions= Set.Poly.empty; distance= None}
            | _ ->
                { directions=
                    Set.Poly.inter left_level.directions right_level.directions
                ; distance=
                    Option.first_some left_level.distance right_level.distance
                }) in
      if List.exists levels ~f:(fun level -> Set.Poly.is_empty level.directions)
      then Independent
      else Dependent levels

(** What is possible for either of two pairs of accesses; a distance is kept
    only when both pairs agree. *)
let join (left : dependence) (right : dependence) : dependence =
  match (left, right) with
  | Unknown, _ | _, Unknown -> Unknown
  | Independent, other | other, Independent -> other
  | Dependent left_levels, Dependent right_levels ->
      Dependent
        (List.map2 left_levels right_levels
           ~f:(fun (left_level : level) right_level ->
             { directions=
                 Set.Poly.union left_level.directions right_level.directions
             ; distance=
                 (if
                    Option.equal Int.equal left_level.distance
                      right_level.distance
                  then left_level.distance
                  else None) }))

(** Whether two accesses to one variable can touch the same element. The
    elements are the same only when the paths are equal at every position, so
    the per-position results are intersected. Two different tuple fields never
    overlap. Accesses with paths of different lengths are [Unknown]. *)
let access_dependence (frame : frame) (source : point access)
    (sink : point access) : dependence =
  if List.compare_lengths source.path sink.path <> 0 then Unknown
  else
    List.fold_left2 source.path sink.path ~init:Unknown
      ~f:(fun merged source_step sink_step ->
        meet merged
          (match (source_step, sink_step) with
          | Subscript (Single source_point), Subscript (Single sink_point) ->
              point_dependence frame source_point sink_point
          | Field source_field, Field sink_field when source_field <> sink_field
            ->
              Independent
          | Subscript _, _ | Field _, _ -> Unknown))

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
    [src] to run first. [Unknown] stays [Unknown]. *)
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
  | (Independent | Unknown) as unchanged -> unchanged
  | Dependent levels -> (
      match restrict levels with
      | Some restricted -> Dependent restricted
      | None -> Independent)

(** The label of the analysed statement. An assignment from before the analysed
    statement is recorded at this label. *)
let root_label : label = 1

(** The dependence from the accesses [sources] to the accesses [sinks]:
    [Independent] only when every pair is independent. [restrict] is applied to
    each pair before the pairs are combined, so that a direction removed for one
    pair is not added back by another. Two [Increment]s are skipped, since
    increments can run in either order. When either list is empty the accesses
    are unknown and the result is [Unknown]. *)
let pair_dependence (frame : frame) ~(restrict : dependence -> dependence)
    (sources : point access list) (sinks : point access list) : dependence =
  if List.is_empty sources || List.is_empty sinks then Unknown
  else
    List.fold_left
      (List.concat_map sources ~f:(fun source ->
           List.map sinks ~f:(fun sink -> (source, sink))))
      ~init:Independent
      ~f:(fun merged ((source : point access), (sink : point access)) ->
        match (source.kind, sink.kind) with
        | Increment, Increment -> merged
        | (Read | Write), _ | Increment, (Read | Write) ->
            join merged (restrict (access_dependence frame source sink)))

(** The loop variable of the loop at [loop]; a [while] loop has none. *)
let for_loopvar
    (statement_map : ((Expr.Typed.t, label) Stmt.Pattern.t * _) LabelMap.t)
    (loop : label) : string option =
  match fst (LabelMap.find loop statement_map) with
  | Stmt.Pattern.For {loopvar; _} -> Some loopvar
  | _ -> None

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
        (for_loopvar statement_map loop))

(** The accesses of the statement at [label]. *)
let accesses_at (statement_map : dep_info_map) (label : label) :
    point Accesses.t =
  (snd (LabelMap.find label statement_map)).accesses

(** The labels in [sources] that may touch an element that [dst_accesses] touch,
    each with the dependence. [src_accesses] gives the accesses of a source, and
    [restrict] limits each pair of accesses, for example to the pairs where the
    source runs first. A source outside the analysed statement has unknown
    accesses and is always kept. *)
let element_edges (statement_map : dep_info_map) ~(dst : label)
    ~(sources : label Set.Poly.t)
    ~(restrict : src:label -> dependence -> dependence)
    ~(src_accesses : label -> point access list)
    ~(dst_accesses : point access list) : (label * dependence) list =
  List.filter_map (Set.Poly.to_list sources) ~f:(fun src ->
      if src = root_label || not (LabelMap.mem src statement_map) then
        Some (src, Unknown)
      else
        match
          pair_dependence
            (common_frame statement_map ~src ~dst)
            ~restrict:(restrict ~src) (src_accesses src) dst_accesses
        with
        | Independent -> None
        | (Unknown | Dependent _) as dep -> Some (src, dep))

(** The assignments to [var] that reach [dst] and may write an element that
    [dst] reads before [dst] runs. *)
let pruned_reaching_defns (statement_map : dep_info_map) (dst : label)
    (var : string) : label Set.Poly.t =
  let _, info = LabelMap.find dst statement_map in
  element_edges statement_map ~dst
    ~sources:(reaching_defn_lookup info.reaching_defn_entry var)
    ~restrict:(ordered_dependence ~dst)
    ~src_accesses:(fun src ->
      (Accesses.of_var var (accesses_at statement_map src)).writes)
    ~dst_accesses:(Accesses.of_var var info.accesses).reads
  |> List.map ~f:fst |> Set.Poly.of_list

(** The variables the statement reads, including the variables read inside
    indices and sizes, and the variables the statement increments. *)
let read_variables (info : node_dep_info) : string Set.Poly.t =
  Set.Poly.of_list (List.map info.accesses.reads ~f:(fun access -> access.var))

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

(** Returns an optional label for the innermost [for] or [while] loop around
    [label]. [parents] holds, for each label, the nearest control-flow statement
    around the label (and the labels of [break] and [continue] statements), so
    the search walks outward one control-flow statement at a time. *)
let rec enclosing_loop statement_map parents (label : label) : label option =
  let pattern_of node = fst (LabelMap.find node statement_map) in
  List.find_opt
    (Set.Poly.to_list (LabelMap.find label parents))
    ~f:(fun parent -> is_ctrl_flow (pattern_of parent))
  |> Option.bind ~f:(fun parent ->
      match pattern_of parent with
      | Stmt.Pattern.For _ | While _ -> Some parent
      | _ -> enclosing_loop statement_map parents parent)

let build_dep_info_map (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    dep_info_map =
  let statement_map =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      stmt in
  let _, preds, parents = build_cf_graphs statement_map in
  let rd_map = mir_reaching_definitions mir stmt in
  let collected =
    LabelMap.map statement_map ~f:(fun (pattern, _) -> Accesses.of_stmt pattern)
  in
  (* the variables some statement inside [stmt] writes; an index that reads one
     of them can change value inside [stmt] *)
  let written_vars =
    LabelMap.fold collected ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:(accesses : Expr.Typed.t Accesses.t) written ->
        Set.Poly.union written
          (Set.Poly.of_list
             (List.map accesses.writes ~f:(fun access -> access.var)))) in
  (* the loop variables of the [for] loops around [label] *)
  let rec loopvars_of label =
    match enclosing_loop statement_map parents label with
    | None -> Set.Poly.empty
    | Some loop ->
        let outer = loopvars_of loop in
        Option.value_map (for_loopvar statement_map loop) ~default:outer
          ~f:(fun loopvar -> Set.Poly.add loopvar outer) in
  LabelMap.mapi statement_map ~f:(fun label (pattern, idx) ->
      let rds = LabelMap.find label rd_map in
      ( pattern
      , { predecessors= LabelMap.find label preds
        ; parents= LabelMap.find label parents
        ; reaching_defn_entry= rds.entry
        ; reaching_defn_exit= rds.exit
        ; loop= enclosing_loop statement_map parents label
        ; accesses=
            Accesses.classify ~loopvars:(loopvars_of label) ~written_vars
              (LabelMap.find label collected)
        ; meta= idx } ))

let log_prob_build_dep_info_map (mir : Program.Typed.t) : dep_info_map =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dep_info_map mir log_prob_stmt

let log_prob_dependency_graph (mir : Program.Typed.t) : dependency_graph =
  let dep_info_map = log_prob_build_dep_info_map mir in
  all_node_dependencies dep_info_map

let read_variables_at (statement_map : dep_info_map) (labels : label Set.Poly.t)
    : string Set.Poly.t =
  Set.Poly.union_map labels ~f:(fun label ->
      read_variables (snd (LabelMap.find label statement_map)))
