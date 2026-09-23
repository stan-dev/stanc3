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
type 'index step = Subscript of 'index Index.t | Field of int
type 'index access = {var: string; path: 'index step list}

(** The [linear] form of [left + right] for [classify_point], or [None] when
    both sides have a symbol or both have a loop variable. *)
let linear_add (left : linear) (right : linear) : linear option =
  let add left_term right_term =
    match (left_term, right_term) with
    | term, None | None, term -> Some term
    | Some _, Some _ -> None in
  Option.bind (add left.symbol right.symbol) ~f:(fun symbol ->
      Option.map (add left.loopvar right.loopvar) ~f:(fun loopvar ->
          {const= left.const + right.const; symbol; loopvar}))

(** The [linear] form of [left - right] for [classify_point], or [None] unless
    each symbol and loop variable of [right] cancels the same one in [left]. *)
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

(** The [point] form of the integer index [expr], which is [Varying] when [expr]
    reads [written_vars] or uses a loop variable nonlinearly. *)
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
        (* the frontend rejects an integer literal that does not fit an int *)
        | None -> symbolic subexpr [@coverage off])
    | FunApp (Operator Plus, [lhs; rhs]) -> combine linear_add lhs rhs
    | FunApp (Operator Minus, [lhs; rhs]) -> combine linear_subtract lhs rhs
    | Var _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Indexed _
     |TupleProjection _ ->
        symbolic subexpr
    (* an integer index holds no other literal, and a promotion only makes a
       real or a complex value *)
    | Lit _ | Promotion _ -> symbolic subexpr [@coverage off] in
  classify expr

(** Building and classifying the [step list] that leads from a variable to the
    element or tuple field an access touches. *)
module Path = struct
  (** The steps of [path] with every index classified by [classify_point], so
      that [Dependence.of_accesses] can compare two paths. *)
  let classify ~loopvars ~written_vars (path : Expr.Typed.t step list) :
      point step list =
    List.map path ~f:(function
      | Subscript index ->
          Subscript (Index.map (classify_point ~loopvars ~written_vars) index)
      | Field field -> Field field)

  (** The variable and path of a read such as [x[i][j].2], or [None] when the
      base is not a variable or a slice comes before the last index list. *)
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
    | FunApp _ | TernaryIf _ -> None
    (* a literal, a boolean and a promotion are never indexed *)
    | Lit _ | EAnd _ | EOr _ | Promotion _ -> None [@coverage off]

  (** The path of the left side of an assignment, so [x[i].2[j] = ...] gives the
      steps [i], [.2] and [j]. *)
  let rec of_lvalue ((lbase, indices) : Expr.Typed.t Stmt.Pattern.lvalue) :
      Expr.Typed.t step list =
    (match lbase with
      | LVariable _ -> []
      | LTupleProjection (inner, field) -> of_lvalue inner @ [Field field])
    @ List.map indices ~f:(fun index -> Subscript index)

  (** The index expressions in [path], which the access reads in addition to the
      indexed variable. *)
  let bounds (path : Expr.Typed.t step list) : Expr.Typed.t list =
    List.concat_map path ~f:(function
      | Subscript index -> Index.bounds index
      | Field _ -> [])
end

module Accesses = struct
  type 'index t =
    { reads: 'index access list
    ; writes: 'index access list
    ; increments: 'index access list }

  (** The accesses of one read of [var] at [path]. *)
  let read (var : string) (path : 'index step list) : 'index t =
    {reads= [{var; path}]; writes= []; increments= []}

  (** The accesses of one write of [var] at [path]. *)
  let write (var : string) (path : 'index step list) : 'index t =
    {reads= []; writes= [{var; path}]; increments= []}

  (** The accesses of one increment of [target], such as [target += ...] or a
      call to an [_lp] function. *)
  let increment_target : 'index t =
    {reads= []; writes= []; increments= [{var= "target"; path= []}]}

  (** The accesses of every part in [parts], kept in the order of [parts]. *)
  let concat (parts : 'index t list) : 'index t =
    { reads= List.concat_map parts ~f:(fun part -> part.reads)
    ; writes= List.concat_map parts ~f:(fun part -> part.writes)
    ; increments= List.concat_map parts ~f:(fun part -> part.increments) }

  (** The accesses in [accesses] to the variable [var] only. *)
  let of_var (var : string) (accesses : 'index t) : 'index t =
    let keep = List.filter ~f:(fun access -> String.equal access.var var) in
    { reads= keep accesses.reads
    ; writes= keep accesses.writes
    ; increments= keep accesses.increments }

  (** The names of the variables that [accesses] read or increment. *)
  let read_vars (accesses : 'index t) : string Set.Poly.t =
    Set.Poly.of_list
      (List.map (accesses.reads @ accesses.increments) ~f:(fun access ->
           access.var))

  (** The names of the variables that [accesses] write or increment. *)
  let written_vars (accesses : 'index t) : string Set.Poly.t =
    Set.Poly.of_list
      (List.map (accesses.writes @ accesses.increments) ~f:(fun access ->
           access.var))

  (** The accesses of [expr] in evaluation order, where [target()] reads
      [target] and an [_lp] or [_jacobian] call increments [target]. *)
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
    | FunApp (kind, _) when increments_target kind ->
        concat [of_children (); increment_target]
    | Lit _ | FunApp _ | TernaryIf _ | EAnd _ | EOr _ | Promotion _ ->
        of_children ()

  (** The accesses of [stmt] without the nested statements, where a declaration
      writes the variable and reads the sizes of the declared type. *)
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
    | TargetPE _ | JacobianPE _ -> concat [of_children (); increment_target]
    | NRFunApp (kind, _) when increments_target kind ->
        concat [of_children (); increment_target]
    | NRFunApp _ | Return _ | IfElse _ | While _ | For _ | Profile _ | Block _
     |SList _ | Break | Continue | Skip ->
        of_children ()

  (** [accesses] with every path classified by [Path.classify] and without the
      reads of [loopvars], which [classify_point] treats as part of an index. *)
  let classify ~loopvars ~written_vars (accesses : Expr.Typed.t t) : point t =
    let classify_paths =
      List.map ~f:(fun access ->
          {access with path= Path.classify ~loopvars ~written_vars access.path})
    in
    { reads=
        classify_paths
          (List.filter accesses.reads ~f:(fun access ->
               not (Set.Poly.mem access.var loopvars)))
    ; writes= classify_paths accesses.writes
    ; increments= classify_paths accesses.increments }
end

type direction = Lt | Eq | Gt
type level = {directions: direction Set.Poly.t; distance: int option}

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

(** The dependence between two accesses, found per index by [of_points] and
    combined across indices by [meet] and across access pairs by [join]. *)
module Dependence = struct
  type t = Independent | Unknown | Dependent of level list

  (** The dependence between two single indices by the ZIV and strong SIV tests
      of Goff, Kennedy and Tseng (1991), which need the same symbol. *)
  let of_points (common_loopvars : string option list) (source : point)
      (sink : point) : t =
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
        | Some distance, Some loopvar
          when List.mem (Some loopvar) ~set:common_loopvars ->
            let direction =
              if distance = 0 then Eq else if distance > 0 then Lt else Gt in
            (* only the level of [loopvar] is known; any direction is possible
               at the other loops *)
            Dependent
              (List.map common_loopvars ~f:(fun var ->
                   if Option.equal String.equal var (Some loopvar) then
                     { directions= Set.Poly.singleton direction
                     ; distance= Some distance }
                   else
                     {directions= Set.Poly.of_list [Lt; Eq; Gt]; distance= None}))
        | Some _, Some _ -> Unknown)
    | Affine _, Affine _ | Varying _, _ | _, Varying _ -> Unknown

  (** The dependence at two index positions of one access pair together, which
      keeps only the iterations that both positions allow. *)
  let meet (left : t) (right : t) : t =
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
                      Set.Poly.inter left_level.directions
                        right_level.directions
                  ; distance=
                      Option.first_some left_level.distance right_level.distance
                  }) in
        if
          List.exists levels ~f:(fun level ->
              Set.Poly.is_empty level.directions)
        then Independent
        else Dependent levels

  (** The dependence of two pairs of accesses together, keeping every iteration
      either pair allows and a distance only when both pairs agree. *)
  let join (left : t) (right : t) : t =
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

  (** The dependence between two accesses to one variable as the [meet] over the
      index positions, where different tuple fields are [Independent]. *)
  let of_accesses (common_loopvars : string option list) (source : point access)
      (sink : point access) : t =
    if List.compare_lengths source.path sink.path <> 0 then Unknown
    else
      List.fold_left2 source.path sink.path ~init:Unknown
        ~f:(fun merged source_step sink_step ->
          meet merged
            (match (source_step, sink_step) with
            | Subscript (Single source_point), Subscript (Single sink_point) ->
                of_points common_loopvars source_point sink_point
            | Field source_field, Field sink_field
              when source_field <> sink_field ->
                Independent
            | Subscript _, _ | Field _, _ -> Unknown))

  (** [dep] restricted to the iterations where the access at [src] runs before
      the access at [dst] (Kennedy and Allen 2001, definition 2.1). *)
  let ordered ~(src : label) ~(dst : label) (dep : t) : t =
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

  type kind = Flow | Anti | Output

  (** The accesses a [kind] dependence starts from, not counting increments. *)
  let source_uses (kind : kind) (accesses : 'index Accesses.t) :
      'index access list =
    match kind with Flow | Output -> accesses.writes | Anti -> accesses.reads

  (** The accesses a [kind] dependence ends at, not counting increments. *)
  let sink_uses (kind : kind) (accesses : 'index Accesses.t) :
      'index access list =
    match kind with Flow -> accesses.reads | Anti | Output -> accesses.writes

  (** The [join] of [restrict] over the [kind] pairs from [source] to [sink],
      where an increment pairs as either use but not with an increment. *)
  let between (common_loopvars : string option list) ~(restrict : t -> t)
      (kind : kind) (source : point Accesses.t) (sink : point Accesses.t) : t =
    let pairs source_list sink_list =
      List.concat_map source_list ~f:(fun source_access ->
          List.map sink_list ~f:(fun sink_access ->
              (source_access, sink_access))) in
    let sources = source_uses kind source in
    let sinks = sink_uses kind sink in
    if
      List.is_empty (sources @ source.increments)
      || List.is_empty (sinks @ sink.increments)
    then Unknown
    else
      List.fold_left
        (pairs sources sinks
        @ pairs sources sink.increments
        @ pairs source.increments sinks)
        ~init:Independent
        ~f:(fun merged (source_access, sink_access) ->
          join merged
            (restrict (of_accesses common_loopvars source_access sink_access)))
end

(** Find all of the reaching definitions of a variable in an RD set *)
let reaching_defn_lookup (rds : reaching_defn Set.Poly.t) (var : string) :
    label Set.Poly.t =
  Set.Poly.map
    (Set.Poly.filter rds ~f:(fun (defined, _) -> String.equal defined var))
    ~f:snd

(** The label of the analysed statement, which also holds every definition from
    before the analysed statement. *)
let root_label : label = 1

(** The loop variable of the loop at [loop], or [None] for a [while] loop. *)
let for_loopvar
    (statement_map : ((Expr.Typed.t, label) Stmt.Pattern.t * _) LabelMap.t)
    (loop : label) : string option =
  match fst (LabelMap.find loop statement_map) with
  | Stmt.Pattern.For {loopvar; _} -> Some loopvar
  | _ -> None

(** The loop variables of the loops around both [src] and [dst], outermost
    first, which give the levels of a [Dependent] result. *)
let common_loopvars (statement_map : dep_info_map) ~(src : label) ~(dst : label)
    : string option list =
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

(** The classified accesses of the statement at [label]. *)
let accesses_at (statement_map : dep_info_map) (label : label) :
    point Accesses.t =
  (snd (LabelMap.find label statement_map)).accesses

(** The labels in [sources] with a [kind] dependence into [dst_accesses], each
    paired with the dependence, where [None] accesses are [Unknown]. *)
let overlapping_sources (statement_map : dep_info_map) ~(dst : label)
    ~(restrict : src:label -> Dependence.t -> Dependence.t)
    (kind : Dependence.kind) ~(sources : (label * point Accesses.t option) list)
    (dst_accesses : point Accesses.t) : (label * Dependence.t) list =
  List.filter_map sources ~f:(fun (src, src_accesses) ->
      match src_accesses with
      | None -> Some (src, Dependence.Unknown)
      | Some known -> (
          match
            Dependence.between
              (common_loopvars statement_map ~src ~dst)
              ~restrict:(restrict ~src) kind known dst_accesses
          with
          | Independent -> None
          | (Unknown | Dependent _) as dep -> Some (src, dep)))

(** The statements whose writes to [var] [dst] may read, the sources of the flow
    dependences into [dst] (Allen and Kennedy 1987). *)
let writes_read_by (statement_map : dep_info_map) ~(dst : label) (var : string)
    : label Set.Poly.t =
  let _, info = LabelMap.find dst statement_map in
  (* a definition from outside the analysed statement is recorded at
     [root_label] and has unknown accesses *)
  let sources =
    List.map
      (Set.Poly.to_list (reaching_defn_lookup info.reaching_defn_entry var))
      ~f:(fun src ->
        ( src
        , if src = root_label then None
          else Some (Accesses.of_var var (accesses_at statement_map src)) ))
  in
  overlapping_sources statement_map ~dst ~restrict:(Dependence.ordered ~dst)
    Flow ~sources
    (Accesses.of_var var info.accesses)
  |> List.map ~f:fst |> Set.Poly.of_list

(** The variables the statement reads or increments, including the variables
    read inside indices and sizes. *)
let read_variables (info : node_dep_info) : string Set.Poly.t =
  Accesses.read_vars info.accesses

let node_immediate_dependencies (statement_map : dep_info_map)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (label : label) :
    label Set.Poly.t =
  let _, info = LabelMap.find label statement_map in
  let rhs_deps =
    Set.Poly.union_map
      (Set.Poly.diff (read_variables info) blockers)
      ~f:(writes_read_by statement_map ~dst:label) in
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
    Set.Poly.fold deps ~init:visited'
      ~f:(node_dependencies_rec statement_map ~blockers)

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
      ~f:(writes_read_by statement_map ~dst:label) in
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

(** The innermost [for] or [while] loop around [label], found by walking outward
    through the control-flow statements in [parents]. *)
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
      ~f:(fun ~key:_ ~data:accesses written ->
        Set.Poly.union written (Accesses.written_vars accesses)) in
  (* the loop variables of the [for] loops around [label] *)
  let rec enclosing_loopvars label =
    match enclosing_loop statement_map parents label with
    | None -> Set.Poly.empty
    | Some loop ->
        let outer = enclosing_loopvars loop in
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
            Accesses.classify ~loopvars:(enclosing_loopvars label) ~written_vars
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

(* ---- The loop dependence graph of one For (design §7.5) ---- *)

(** How the sink depends on the source: reads what the source wrote, writes what
    the source read, writes what the source wrote, or both have effects. *)
type dep_kind = Flow | Anti | Output | Effects

(** [src] executes no later than [dst] in the original loop (Kennedy and Allen
    2001, Definition 2.1). [dep] is the element test between the two leaves'
    accesses to [var] at the analysed loop's level, restricted to that order;
    [var] is [None] for [Effects]. *)
type edge =
  {src: label; dst: label; var: string option; kind: dep_kind; dep: Dependence.t}

(** The graph of one [For]: the leaves of the body in lexical order, the edges
    between them, and the variables some leaf reads (for [is_cyclic]). *)
type loop_graph =
  {leaves: label list; edges: edge list; read_vars: string Set.Poly.t}

(** [dep] at the innermost level alone, the outer loops held at one iteration
    (Allen and Kennedy 1987 §5.2); [Independent] when an outer level lacks [Eq].
*)
let rec innermost_level (dep : Dependence.t) : Dependence.t =
  match dep with
  | Independent | Unknown | Dependent ([] | [_]) -> dep
  | Dependent (outer :: inner) ->
      if Set.Poly.mem Eq outer.directions then innermost_level (Dependent inner)
      else Independent

(** The statements under [label] with [SList] nesting and scope-free [Block]
    nesting flattened. A [Profile] and a [Block] that declares a variable stay
    one leaf: the vectorizer re-emits a leaf as one unit, so the profile and the
    scope stay around their statements. *)
let rec flattened (statement_map : dep_info_map) (label : label) : label list =
  let declares child =
    match fst (LabelMap.find child statement_map) with
    | Stmt.Pattern.Decl _ -> true
    | _ -> false in
  match fst (LabelMap.find label statement_map) with
  | Stmt.Pattern.SList children ->
      List.concat_map children ~f:(flattened statement_map)
  | Block children when not (List.exists children ~f:declares) ->
      List.concat_map children ~f:(flattened statement_map)
  | _ -> [label]

(** The leaf statements of the [For] at [loop], in lexical order: the body's own
    statements flattened, so a nested loop, an [if], a profile or a declaring
    scope is one leaf. [[]] when [loop] is not a [For]. *)
let loop_leaves (statement_map : dep_info_map) (loop : label) : label list =
  match fst (LabelMap.find loop statement_map) with
  | Stmt.Pattern.For {body; _} -> (
      match fst (LabelMap.find body statement_map) with
      | Block children | SList children ->
          List.concat_map children ~f:(flattened statement_map)
      | _ -> [body])
  | _ -> []

(** [label] and every label below, in pre-order. *)
let subtree_labels (statement_map : dep_info_map) (label : label) : label list =
  let rec collect above label =
    Stmt.Pattern.fold Fun.const collect (label :: above)
      (fst (LabelMap.find label statement_map)) in
  List.rev (collect [] label)

(** The accesses of the statement at [label] and of every statement below, so a
    leaf stands for the whole subtree; a nested loop's subscripts still name
    that loop's variable, which the element test treats as ranging freely. *)
let subtree_accesses (statement_map : dep_info_map) (label : label) :
    point Accesses.t =
  Accesses.concat
    (List.map
       (subtree_labels statement_map label)
       ~f:(accesses_at statement_map))

(** The statement at [label], children rebuilt from the map. *)
let statement_at (statement_map : dep_info_map) (label : label) : Stmt.Located.t
    =
  build_recursive_statement
    (fun pattern (info : node_dep_info) -> Stmt.{pattern; meta= info.meta})
    statement_map label

(** Whether the statement at [leaf], or one below, prints, rejects, draws random
    numbers or calls a user function, whose body may do any of these: the access
    model does not see effects, so such leaves keep their order. *)
let has_effects (statement_map : dep_info_map) (leaf : label) : bool =
  Stmt.Helpers.contains_fn_kind
    (function
      | UserDefined _
       |StanLib (_, FnRng, _)
       |CompilerInternal (FnPrint | FnReject | FnFatalError) ->
          true
      | CompilerInternal internal -> Internal_fun.can_side_effect internal
      | StanLib _ | Operator _ -> false)
    (statement_at statement_map leaf)

(** The dependence graph of the [For] at [loop]: for each destination leaf and
    variable, [overlapping_sources] over the leaves for the flow, output and
    anti pairs, each pair reduced to the analysed loop's level
    ([innermost_level], the outer loops stay sequential) and to the order in
    which the leaves execute ([Dependence.ordered]). With [dst] ranging over the
    leaves, the pair [(i, j)], [i < j], is seen as [dst = j] (edge [i -> j] when
    the level admits [Lt] or [Eq]) and as [dst = i] (edge [j -> i] when the
    reversed pair admits [Lt]); a leaf against itself keeps only a loop-carried
    [Lt], so a recurrence [v[n] = v[n-1] + u[n]] is a self flow edge with
    distance 1 and [a[n] = a[n] + 1] has none. A simple leaf is not an anti
    source for itself: a vector statement fetches every input before storing
    (Allen and Kennedy 1987 p. 494); a scope or profile leaf is widened
    statement by statement, so that exemption does not apply. The flow sources
    are the leaves holding a definition of the variable that reaches a statement
    of the destination ([reaching_defn_entry], the reaching-definitions layer),
    so a whole-variable redefinition between two statements drops the earlier
    writer. Effectful leaves get [Effects] edges both ways. *)
let build_loop_graph (statement_map : dep_info_map) ~(loop : label) : loop_graph
    =
  let leaves = loop_leaves statement_map loop in
  let accesses =
    List.map leaves ~f:(fun leaf -> (leaf, subtree_accesses statement_map leaf))
  in
  let accesses_of leaf var = Accesses.of_var var (List.assoc leaf accesses) in
  (* whether [leaf] has an access that [uses] picks or an increment of [var], so
     neither side is empty *)
  let touches leaf var ~uses =
    let of_var = accesses_of leaf var in
    not (List.is_empty (uses of_var @ of_var.increments)) in
  (* the leaf owning each label of the body *)
  let leaf_of =
    LabelMap.of_list
      (List.concat_map leaves ~f:(fun leaf ->
           List.map (subtree_labels statement_map leaf) ~f:(fun label ->
               (label, leaf)))) in
  (* the leaves with a definition of [var] reaching some statement of [dst] *)
  let reaching_writers dst var =
    List.concat_map (subtree_labels statement_map dst) ~f:(fun label ->
        Set.Poly.to_list
          (reaching_defn_lookup
             (snd (LabelMap.find label statement_map)).reaching_defn_entry var))
    |> List.filter_map ~f:(fun label -> LabelMap.find_opt label leaf_of) in
  let compound leaf =
    match fst (LabelMap.find leaf statement_map) with
    | Stmt.Pattern.Block _ | Profile _ -> true
    | _ -> false in
  let edges_into dst =
    let dst_leaf = List.assoc dst accesses in
    let vars =
      Set.Poly.to_list
        (Set.Poly.union
           (Accesses.read_vars dst_leaf)
           (Accesses.written_vars dst_leaf)) in
    List.concat_map vars ~f:(fun var ->
        let writers = reaching_writers dst var in
        List.concat_map
          [ (Flow, Dependence.Flow); (Output, Dependence.Output)
          ; (Anti, Dependence.Anti) ]
          ~f:(fun (kind, dependence_kind) ->
            if
              not (touches dst var ~uses:(Dependence.sink_uses dependence_kind))
            then []
            else
              let sources =
                List.filter_map leaves ~f:(fun src ->
                    Option.some_if
                      (touches src var
                         ~uses:(Dependence.source_uses dependence_kind)
                      &&
                      match kind with
                      | Flow -> List.mem src ~set:writers
                      | Anti -> src <> dst || compound src
                      | Output | Effects -> true)
                      (src, Some (accesses_of src var))) in
              overlapping_sources statement_map ~dst
                ~restrict:(fun ~src dep ->
                  Dependence.ordered ~src ~dst (innermost_level dep))
                dependence_kind ~sources (accesses_of dst var)
              |> List.map ~f:(fun (src, dep) ->
                  {src; dst; var= Some var; kind; dep}))) in
  let effectful = List.filter leaves ~f:(has_effects statement_map) in
  let effect_edges =
    List.concat_map effectful ~f:(fun src ->
        List.filter_map effectful ~f:(fun dst ->
            Option.some_if (src <> dst)
              {src; dst; var= None; kind= Effects; dep= Dependence.Unknown}))
  in
  let key (edge : edge) = (edge.src, edge.dst, edge.var, edge.kind) in
  { leaves
  ; edges=
      List.sort
        (List.concat_map leaves ~f:edges_into @ effect_edges)
        ~cmp:(fun left right -> compare (key left) (key right))
  ; read_vars=
      Set.Poly.union_list
        (List.map accesses ~f:(fun (_, leaf_accesses) ->
             Accesses.read_vars leaf_accesses)) }

(** The pi-blocks of [graph] (Allen and Kennedy 1987 §5.2): the strongly
    connected components by Tarjan's algorithm (Tarjan 1972, as LLVM's
    [scc_iterator]), members in lexical order, then Kahn's algorithm on the
    condensation with ties broken by the earliest first member, so a body with
    only forward edges comes back in original order. *)
let pi_blocks (graph : loop_graph) : label list list =
  let succs =
    List.fold_left graph.edges ~init:LabelMap.empty
      ~f:(fun succs (edge : edge) ->
        LabelMap.add succs ~key:edge.src
          ~data:(edge.dst :: LabelMap.find_multi edge.src succs)) in
  (* [number] is the visit order, [low] the smallest number reachable through
     leaves still on [stack], [block] the first member of a finished leaf's
     component *)
  let number = Hashtbl.create 16 and low = Hashtbl.create 16 in
  let block = Hashtbl.create 16 and stack = ref [] and components = ref [] in
  let lower leaf value =
    Hashtbl.add low ~key:leaf ~data:(min (Hashtbl.find low leaf) value) in
  let rec connect leaf =
    let visit = Hashtbl.length number in
    Hashtbl.add number ~key:leaf ~data:visit;
    Hashtbl.add low ~key:leaf ~data:visit;
    stack := leaf :: !stack;
    List.iter (LabelMap.find_multi leaf succs) ~f:(fun next ->
        if not (Hashtbl.mem number next) then (
          connect next;
          lower leaf (Hashtbl.find low next))
        else if not (Hashtbl.mem block next) then
          lower leaf (Hashtbl.find number next));
    if Hashtbl.find low leaf = visit then (
      let rec pop members =
        match !stack with
        | [] -> members
        | top :: rest ->
            stack := rest;
            if top = leaf then top :: members else pop (top :: members) in
      let members = List.sort (pop []) ~cmp:compare in
      let first = List.hd_exn members in
      components := (first, members) :: !components;
      List.iter members ~f:(fun member ->
          Hashtbl.add block ~key:member ~data:first)) in
  List.iter graph.leaves ~f:(fun leaf ->
      if not (Hashtbl.mem number leaf) then connect leaf);
  (* the edges of the condensation, by first member *)
  let crossing =
    List.filter_map graph.edges ~f:(fun (edge : edge) ->
        let src = Hashtbl.find block edge.src
        and dst = Hashtbl.find block edge.dst in
        Option.some_if (src <> dst) (src, dst)) in
  (* a block is ready once no waiting block has an edge into it; emitting a
     block removes its outgoing edges *)
  let rec emit waiting crossing =
    let ready =
      List.filter waiting ~f:(fun (first, _) ->
          not (List.exists crossing ~f:(fun (_, dst) -> dst = first))) in
    match
      List.min_elt ready ~cmp:(fun (left, _) (right, _) -> compare left right)
    with
    | None -> []
    | Some (first, members) ->
        members
        :: emit
             (List.filter waiting ~f:(fun (other, _) -> other <> first))
             (List.filter crossing ~f:(fun (src, _) -> src <> first)) in
  emit !components crossing

(** A block stays a sequential loop when two leaves depend on each other or one
    depends on an earlier iteration of itself. One exception: a leaf whose self
    dependences are all output dependences on a variable no leaf reads is not
    cyclic. For a scatter [a[idx[n]] = x[n]] Stan's indexed assignment stores a
    multi-index in order, so one vector statement writes the elements in the
    loop's order; a whole-variable write has no vector form and stays in a loop
    regardless. *)
let is_cyclic (graph : loop_graph) (block : label list) : bool =
  match block with
  | [leaf] ->
      List.exists graph.edges ~f:(fun (edge : edge) ->
          edge.src = leaf && edge.dst = leaf
          && (edge.kind <> Output
             || Option.value_map edge.var ~default:true ~f:(fun var ->
                 Set.Poly.mem var graph.read_vars)))
  | _ -> true

(** Whether some edge leaves a leaf of [from] for a leaf of [into]. *)
let edge_between (graph : loop_graph) ~(from : label list) ~(into : label list)
    : bool =
  List.exists graph.edges ~f:(fun (edge : edge) ->
      List.mem edge.src ~set:from && List.mem edge.dst ~set:into)

(** [independent], [unknown], [{<,=}], [{<} d=1]; one group per level. *)
let pp_dependence ppf (dep : Dependence.t) =
  let pp_direction ppf direction =
    Fmt.string ppf (match direction with Lt -> "<" | Eq -> "=" | Gt -> ">")
  in
  let pp_level ppf {directions; distance} =
    Fmt.pf ppf "{%a}"
      Fmt.(list ~sep:(any ",") pp_direction)
      (Set.Poly.to_list directions);
    Option.iter distance ~f:(Fmt.pf ppf " d=%d") in
  match dep with
  | Independent -> Fmt.string ppf "independent"
  | Unknown -> Fmt.string ppf "unknown"
  | Dependent levels -> Fmt.(list ~sep:(any " ") pp_level) ppf levels

(** [S0]: the leaf's number in the report, by position in the body. *)
let pp_position (graph : loop_graph) ppf (label : label) =
  Fmt.pf ppf "S%d"
    (Option.value ~default:(-1)
       (List.find_index graph.leaves ~f:(fun leaf -> leaf = label)))

(** [S0 -> S1 muj {=} d=0 (flow)] or [S0 -> S2 (effects)]. *)
let pp_edge (graph : loop_graph) ppf (edge : edge) =
  Fmt.pf ppf "%a -> %a" (pp_position graph) edge.src (pp_position graph)
    edge.dst;
  match edge.var with
  | None -> Fmt.string ppf " (effects)"
  | Some var ->
      Fmt.pf ppf " %s %a (%s)" var pp_dependence edge.dep
        (match edge.kind with
        | Flow -> "flow"
        | Anti -> "anti"
        | Output -> "output"
        | Effects -> "effects")

(** The statement at [label] on one line and truncated, so a leaf reads as one
    row. *)
let pp_leaf (statement_map : dep_info_map) ppf (label : label) =
  let unbroken =
    Format.asprintf "%t" (fun ppf ->
        Format.pp_set_margin ppf 100_000;
        Stmt.Located.pp ppf (statement_at statement_map label)) in
  let one_line =
    String.split_on_char ~sep:'\n' unbroken
    |> List.map ~f:String.trim |> String.concat ~sep:" " in
  let limit = 96 in
  if String.length one_line > limit then
    Fmt.pf ppf "%s..." (String.sub one_line ~pos:0 ~len:limit)
  else Fmt.string ppf one_line

(** One indented line per leaf ([S0  stmt], with the leaf's outcome after it
    when [outcomes] has one), then [edges: ...] and [blocks: [S0] [S1 S2]cyclic]
    in emission order: the body of a loop's report. *)
let pp_graph (statement_map : dep_info_map) (outcomes : (label * string) list)
    ppf (graph : loop_graph) =
  List.iteri graph.leaves ~f:(fun number leaf ->
      Fmt.pf ppf "  S%d  %a%a@." number (pp_leaf statement_map) leaf
        Fmt.(option (fun ppf -> pf ppf "   %s"))
        (List.assoc_opt leaf outcomes));
  let pp_edges ppf = function
    | [] -> Fmt.string ppf "none"
    | edges -> Fmt.(list ~sep:(any "; ") (pp_edge graph)) ppf edges in
  let pp_block ppf block =
    Fmt.pf ppf "[%a]%s"
      Fmt.(list ~sep:(any " ") (pp_position graph))
      block
      (if is_cyclic graph block then "cyclic" else "") in
  Fmt.pf ppf "  edges: %a@.  blocks: %a@." pp_edges graph.edges
    Fmt.(list ~sep:(any " ") pp_block)
    (pi_blocks graph)
