(* The interface file describes the analysis and the running example. This file
   uses the terms below.

   An access is one read, write or increment of a variable. The path of an
   access lists the subscripts and tuple fields that select an element of the
   variable ({!Accesses}).

   A definition records a write as the access of the write, the label of the
   writing statement and the loops around that statement. A definition reaches a
   statement if the program can go from the write to the statement with no write
   between them that overwrites the same element. Reaching definitions
   ({!Monotone_framework.reaching_definitions_mfp}) finds these definitions.

   A later write kills an earlier definition when the later write overwrites
   each element that the earlier write wrote. The killed definition no longer
   reaches later statements. An element kill compares the paths of two writes to
   single elements, such as [theta[n, 1]], to decide this.

   The controls of a statement are the ifs and loops around the statement, and
   the jumps ([break], [continue] and [return]) that can skip the statement
   ({!Dataflow_types.controls}).

   Each reaching definition of a variable that a statement reads is a candidate
   writer for the read, and the statement that reads is the reader.
   {!Dependence} rejects a candidate writer that can never supply the value that
   the reader uses. *)

open Std
open Middle
open Dataflow_types
open Dataflow_utils
open Monotone_framework_sigs
open Monotone_framework

type reaching_definition =
  {access: Accesses.access; statement_id: label option; loop_ids: label list}

type stmt_info =
  { id: label
  ; pattern: (Expr.Typed.t, label) Stmt.Pattern.t
  ; meta: Location_span.t
  ; controls: controls
  ; loop_ids: label list
  ; accesses: Accesses.t
  ; reaching_definitions: reaching_definition Set.Poly.t
  ; writers: (string * label) Set.Poly.t }

(** The [is_killed] callback for reaching definitions. A write at [killer] kills
    a definition when the write must overwrite the element that the definition
    wrote, by one of two rules. An increment of a variable kills every earlier
    definition of the variable, so the definition of [target] is always the
    latest increment. *)
let is_killed ~(statements : stmt_info LabelMap.t) ~(killer : label)
    (killed : reaching_definition) : bool =
  let killer_statement = LabelMap.find killer statements in
  (* Rule B of 2: [killer] writes the element of [killed] in the same iteration
     of the innermost loop around [killed]. [killer] comes later in source order
     and has the same loops. Each control of [killer], and of the innermost
     loop, is also a control of [killed]. The loops are found last, after the
     cheaper checks. A definition from outside has no label and no iteration. *)
  let is_killed_in_same_iteration
      (killer_path : Affine_subscript.t Accesses.step list) : bool =
    match killed.statement_id with
    | None -> false
    | Some killed_id -> (
        let killed_statement = LabelMap.find killed_id statements in
        (* Whether each if, loop and jump that decides if [statement_id] runs
           also decides if [killed] runs. *)
        let is_covered_by_killed (statement_id : label) : bool =
          let covered = (LabelMap.find statement_id statements).controls in
          List.for_all covered.enclosing ~f:(fun enclosing ->
              List.mem enclosing ~set:killed_statement.controls.enclosing)
          && Set.Poly.subset covered.jump_ids killed_statement.controls.jump_ids
        in
        killed_id < killer
        && Dependence.is_overwritten ~is_same_iteration:true killer_path
             killed.access.path
        &&
        match List.rev killed.loop_ids with
        | innermost :: _ ->
            List.equal ~eq:Int.equal killer_statement.loop_ids killed.loop_ids
            && is_covered_by_killed killer
            && is_covered_by_killed innermost
        | [] -> false) in
  List.exists killer_statement.accesses ~f:(fun (access : Accesses.access) ->
      String.equal access.var killed.access.var
      &&
      match access.kind with
      | Read -> false
      | Increment -> true
      | Write ->
          (* Rule A of 2: [killer] writes the element of [killed] in every
             iteration. A write of the whole variable has an empty path and
             overwrites every element, including an outside definition. *)
          Dependence.is_overwritten ~is_same_iteration:false access.path
            killed.access.path
          || is_killed_in_same_iteration access.path)

(** Runs reaching definitions on the forward flowgraph of [statements]. The
    definitions from outside are the input and output variables of [mir], each
    statement adds a definition for each write or increment in the accesses of
    the statement, and {!is_killed} removes definitions. Returns the definitions
    that reach the node of each label. *)
let compute_reaching_definitions (mir : Program.Typed.t)
    (statements : stmt_info LabelMap.t) :
    reaching_definition Set.Poly.t LabelMap.t =
  let flowgraph, _ =
    statements
    |> LabelMap.map ~f:(fun {pattern; meta; _} -> (pattern, meta))
    |> Monotone_framework.forward_flowgraph_of_statement_map in
  let (module Flowgraph) = flowgraph in
  let outside_definition (name, _, _) : reaching_definition =
    { access= {var= name; path= []; kind= Write}
    ; statement_id= None
    ; loop_ids= [] } in
  let outside_definitions =
    Set.Poly.of_list
      (List.map mir.input_vars ~f:outside_definition
      @ List.map mir.output_vars ~f:outside_definition) in
  let definitions_of (statement_id : label) : reaching_definition Set.Poly.t =
    let statement = LabelMap.find statement_id statements in
    let definition_of_access (access : Accesses.access) =
      match access.kind with
      | Write | Increment ->
          Some
            { access
            ; statement_id= Some statement.id
            ; loop_ids= statement.loop_ids }
      | Read -> None in
    statement.accesses
    |> List.filter_map ~f:definition_of_access
    |> Set.Poly.of_list in
  let definitions_at_entry_and_exit =
    reaching_definitions_mfp ~initial:outside_definitions ~gen:definitions_of
      ~is_killed:(is_killed ~statements)
      (module Flowgraph) in
  LabelMap.map definitions_at_entry_and_exit ~f:(fun definitions ->
      definitions.entry)

let is_flow_dependence (common_loop_ids : label list) (is_writer_first : bool)
    (write : Accesses.access) (read : Accesses.access) : bool =
  String.equal write.var read.var
  && (match (write.kind, read.kind) with
    | (Write | Increment), Read | Write, Increment -> true
    | Read, _ | Increment, Increment | _, Write -> false)
  && Dependence.is_lexicographically_positive common_loop_ids is_writer_first
       write.path read.path

let writers_of_statement (statement : stmt_info) : (string * label) Set.Poly.t =
  let reads =
    List.filter statement.accesses ~f:(fun (access : Accesses.access) ->
        match access.kind with Read | Increment -> true | Write -> false) in
  Set.Poly.union_map statement.reaching_definitions ~f:(fun definition ->
      match definition.statement_id with
      | None -> Set.Poly.empty
      | Some writer_id ->
          let common_loop_ids =
            List.filter definition.loop_ids ~f:(fun loop_id ->
                List.mem loop_id ~set:statement.loop_ids) in
          let is_writer_first = writer_id < statement.id in
          let filter_flow_dependence =
            is_flow_dependence common_loop_ids is_writer_first definition.access
          in
          reads
          |> List.filter ~f:filter_flow_dependence
          |> List.map ~f:(fun (read : Accesses.access) -> (read.var, writer_id))
          |> Set.Poly.of_list)

(** Returns the labels of the direct dependences of the statement, the control
    parents first and the writers of the variables the statement reads second.
    [vars], when given, keeps only the writers of those variables, and the
    writers of the variables in [blockers] are left out. *)
let direct_dependence_ids ?(blockers : string Set.Poly.t = Set.Poly.empty)
    ?(vars : string Set.Poly.t option) (statement : stmt_info) :
    label Set.Poly.t * label Set.Poly.t =
  let is_followed var =
    (not (Set.Poly.mem var blockers))
    && Option.value_map vars ~default:true ~f:(Set.Poly.mem var) in
  let writer_ids =
    Set.Poly.filter_map statement.writers ~f:(fun (var, writer_id) ->
        if is_followed var then Some writer_id else None) in
  (control_parent_ids statement.controls, writer_ids)

(** Adds [statement_id] and the direct and transitive dependences of
    [statement_id] to [visited_ids], without following the definitions of
    [blockers]. *)
let rec add_transitive_dependence_ids (statements : stmt_info LabelMap.t)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (statement_id : label)
    (visited_ids : label Set.Poly.t) : label Set.Poly.t =
  if Set.Poly.mem statement_id visited_ids then visited_ids
  else
    (* The visited set is the result and includes the start label. Each
       dependence is a label, so the walk looks up the record at that label. *)
    let visited_with_statement_ids = Set.Poly.add statement_id visited_ids in
    let control_ids, writer_ids =
      direct_dependence_ids ~blockers (LabelMap.find statement_id statements)
    in
    Set.Poly.union control_ids writer_ids
    |> Set.Poly.fold ~init:visited_with_statement_ids
         ~f:(add_transitive_dependence_ids statements ~blockers)

let transitive_dependence_ids (statements : stmt_info LabelMap.t)
    (statement_id : label) : label Set.Poly.t =
  add_transitive_dependence_ids statements statement_id Set.Poly.empty

let var_dependence_ids (statements : stmt_info LabelMap.t)
    ?(blockers : string Set.Poly.t = Set.Poly.empty) (vars : string Set.Poly.t)
    (statement_id : label) : label Set.Poly.t =
  let control_ids, writer_ids =
    direct_dependence_ids ~blockers ~vars
      (LabelMap.find statement_id statements) in
  Set.Poly.union control_ids writer_ids
  |> Set.Poly.fold ~init:Set.Poly.empty
       ~f:(add_transitive_dependence_ids statements ~blockers)

let used_vars_at (statements : stmt_info LabelMap.t)
    (statement_ids : label Set.Poly.t) : string Set.Poly.t =
  Set.Poly.union_map statement_ids ~f:(fun statement_id ->
      Accesses.used_vars (LabelMap.find statement_id statements).accesses)

let build_dependence_info (mir : Program.Typed.t) (stmt : Stmt.Located.t) :
    stmt_info LabelMap.t =
  let statement_map = build_statement_map stmt in
  let control_ids = build_controls statement_map in
  let add_defined_vars ~key:_ ~data:(pattern, _) running_defined_vars =
    pattern
    |> Accesses.of_statement ~classify:(fun _ -> None)
    |> Accesses.defined_vars
    |> Set.Poly.union running_defined_vars in
  let defined_vars =
    LabelMap.fold statement_map ~init:Set.Poly.empty ~f:add_defined_vars in
  let statements_without_definitions =
    LabelMap.mapi statement_map ~f:(fun statement_id (pattern, meta) ->
        let controls = LabelMap.find statement_id control_ids in
        let loopvar_and_id loop_id =
          match fst (LabelMap.find loop_id statement_map) with
          | For {loopvar; _} -> Some (loopvar, loop_id)
          | _ -> None in
        let loop_ids = enclosing_loop_ids statement_id pattern controls in
        let for_loops = List.filter_map loop_ids ~f:loopvar_and_id in
        let is_loop_var_read (access : Accesses.access) =
          match access.kind with
          | Read ->
              List.exists for_loops ~f:(fun (loopvar, _) ->
                  String.equal loopvar access.var)
          | Write | Increment -> false in
        let accesses =
          pattern
          |> Accesses.of_statement
               ~classify:(Affine_subscript.of_expr ~for_loops ~defined_vars)
          |> List.filter ~f:(fun access -> not (is_loop_var_read access)) in
        { id= statement_id
        ; pattern
        ; meta
        ; controls
        ; loop_ids
        ; accesses
        ; reaching_definitions= Set.Poly.empty
        ; writers= Set.Poly.empty }) in
  (* The definitions and the kills use the classified accesses, so the records
     come first, with empty definition and writer sets. The dataflow fills the
     definitions, and the writers come from the definitions. *)
  let reaching_definitions =
    compute_reaching_definitions mir statements_without_definitions in
  LabelMap.mapi statements_without_definitions ~f:(fun statement_id statement ->
      let with_definitions =
        { statement with
          reaching_definitions= LabelMap.find statement_id reaching_definitions
        } in
      {with_definitions with writers= writers_of_statement with_definitions})

let build_log_prob_dependence_info (mir : Program.Typed.t) :
    stmt_info LabelMap.t =
  let log_prob_stmt =
    Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  build_dependence_info mir log_prob_stmt

(* ---- The loop dependence graph of one For (design section 7.5) ---- *)

(** How the destination depends on the source: reads what the source wrote,
    writes what the source read, writes what the source wrote, or both have
    effects. *)
type dep_kind = Flow | Anti | Output | Effects

(** [src] executes no later than [dst] in the original loop (Kennedy and Allen
    2001, Definition 2.1). [distance] is the number of iterations of the
    analysed loop from the source access to the destination access: [Some 0] for
    the same iteration, positive for a later one, [None] when the subscript test
    cannot tell. [var] is [None] for [Effects], and so is [distance]. *)
type edge =
  { src: label
  ; dst: label
  ; var: string option
  ; kind: dep_kind
  ; distance: int option }

(** The graph of one [For]: the leaves of the body in lexical order, the edges
    between them, and the variables some leaf reads (for [is_cyclic]). *)
type loop_graph =
  {leaves: label list; edges: edge list; read_vars: string Set.Poly.t}

(** The statements under [label] with [SList] nesting and scope-free [Block]
    nesting flattened. A [Profile] and a [Block] that declares a variable stay
    one leaf: the vectorizer re-emits a leaf as one unit, so the profile and the
    scope stay around their statements. *)
let rec flattened (statements : stmt_info LabelMap.t) (label : label) :
    label list =
  let declares child =
    match (LabelMap.find child statements).pattern with
    | Stmt.Pattern.Decl _ -> true
    | _ -> false in
  match (LabelMap.find label statements).pattern with
  | Stmt.Pattern.SList children ->
      List.concat_map children ~f:(flattened statements)
  | Block children when not (List.exists children ~f:declares) ->
      List.concat_map children ~f:(flattened statements)
  | _ -> [label]

(** The leaf statements of the [For] at [loop], in lexical order: the body's own
    statements flattened, so a nested loop, an [if], a profile or a declaring
    scope is one leaf. [[]] when [loop] is not a [For]. *)
let loop_leaves (statements : stmt_info LabelMap.t) (loop : label) : label list
    =
  match (LabelMap.find loop statements).pattern with
  | Stmt.Pattern.For {body; _} -> (
      match (LabelMap.find body statements).pattern with
      | Block children | SList children ->
          List.concat_map children ~f:(flattened statements)
      | _ -> [body])
  | _ -> []

(** [label] and every label below, in pre-order. *)
let subtree_labels (statements : stmt_info LabelMap.t) (label : label) :
    label list =
  let rec collect above label =
    Stmt.Pattern.fold Fun.const collect (label :: above)
      (LabelMap.find label statements).pattern in
  List.rev (collect [] label)

(** The accesses of the statement at [label] and of every statement below, so a
    leaf stands for the whole subtree. A subscript in a nested loop names that
    loop, which is around neither leaf, so the subscript test treats the
    subscript as ranging freely. *)
let subtree_accesses (statements : stmt_info LabelMap.t) (label : label) :
    Accesses.t =
  List.concat_map (subtree_labels statements label) ~f:(fun label ->
      (LabelMap.find label statements).accesses)

(** The statement at [label], children rebuilt from the map. *)
let statement_at (statements : stmt_info LabelMap.t) (label : label) :
    Stmt.Located.t =
  build_recursive_statement
    (fun pattern (statement : stmt_info) ->
      Stmt.{pattern; meta= statement.meta})
    (LabelMap.map statements ~f:(fun statement ->
         (statement.pattern, statement)))
    label

(** Whether the statement at [leaf], or one below, prints, rejects, draws random
    numbers or calls a user function, whose body may do any of these: the access
    model does not see effects, so such leaves keep their order. *)
let has_effects (statements : stmt_info LabelMap.t) (leaf : label) : bool =
  Stmt.Helpers.contains_fn_kind
    (function
      | UserDefined _
       |StanLib (_, FnRng)
       |CompilerInternal (FnPrint | FnReject | FnFatalError) ->
          true
      | CompilerInternal internal -> Internal_fun.can_side_effect internal
      | StanLib _ | Operator _ -> false)
    (statement_at statements leaf)

(** The distance at the analysed loop between a source access and a destination
    access, when the two can select the same element and the source can run
    first: [None] for no dependence, [Some None] for a dependence at an unknown
    distance, [Some (Some d)] at distance [d]. [common_loop_ids] holds the loops
    around the analysed loop and that loop last, so the last distance of the
    vector is the one at the analysed loop. An outer loop must allow the same
    iteration, which holds the outer loops at one iteration (Allen and Kennedy
    1987, section 5.2). A positive or unknown distance lets the source run first
    in every case; a zero distance only when the source leaf comes before the
    destination leaf, so a leaf against itself keeps only a loop-carried
    dependence: [v[n] = v[n - 1] + u[n]] is a self flow edge with distance 1 and
    [a[n] = a[n] + 1] has none. *)
let distance_at_loop ~(common_loop_ids : label list) ~(is_src_first : bool)
    (src_path : Affine_subscript.t Accesses.step list)
    (dst_path : Affine_subscript.t Accesses.step list) : int option option =
  match Dependence.test_dependence common_loop_ids src_path dst_path with
  | Independent -> None
  | Distance_vector distances -> (
      match List.rev distances with
      | [] -> None
      | at_loop :: outer ->
          let outer_same_iteration =
            List.for_all outer ~f:(function
              | Some 0 | None -> true
              | Some _ -> false) in
          let src_runs_first =
            match at_loop with
            | None -> true
            | Some distance -> distance > 0 || (distance = 0 && is_src_first)
          in
          if outer_same_iteration && src_runs_first then Some at_loop else None)

(** The dependence graph of the [For] at [loop]: for each destination leaf and
    variable, each source leaf is tested for the flow, output and anti pairs of
    accesses at the analysed loop's level ({!distance_at_loop}). The pairs of
    one edge join to one distance, unknown when the pairs disagree. Two
    increments commute, so an increment never pairs with an increment. A simple
    leaf is not an anti source for itself: a vector statement fetches every
    input before storing (Allen and Kennedy 1987 p. 494); a scope or profile
    leaf is widened statement by statement, so that exemption does not apply.
    The flow sources are the leaves holding a definition of the variable that
    reaches a statement of the destination, so a whole-variable redefinition
    between two statements drops the earlier writer. Effectful leaves get
    [Effects] edges both ways. *)
let build_loop_graph (statements : stmt_info LabelMap.t) ~(loop : label) :
    loop_graph =
  (* a loop counts as inside the loop that the loop starts, so the loop is the
     last of the loops around the loop statement *)
  let common_loop_ids = (LabelMap.find loop statements).loop_ids in
  let leaves = loop_leaves statements loop in
  let accesses =
    List.map leaves ~f:(fun leaf -> (leaf, subtree_accesses statements leaf))
  in
  let accesses_of leaf var =
    List.filter (List.assoc leaf accesses) ~f:(fun (access : Accesses.access) ->
        String.equal access.var var) in
  (* the leaf owning each label of the body *)
  let leaf_of =
    LabelMap.of_list
      (List.concat_map leaves ~f:(fun leaf ->
           List.map (subtree_labels statements leaf) ~f:(fun label ->
               (label, leaf)))) in
  (* the leaves with a definition of [var] reaching some statement of [dst] *)
  let reaching_writers dst var =
    List.concat_map (subtree_labels statements dst) ~f:(fun label ->
        Set.Poly.to_list (LabelMap.find label statements).reaching_definitions
        |> List.filter_map ~f:(fun definition ->
            if String.equal definition.access.var var then
              definition.statement_id
            else None))
    |> List.filter_map ~f:(fun label -> LabelMap.find_opt label leaf_of) in
  let compound leaf =
    match (LabelMap.find leaf statements).pattern with
    | Stmt.Pattern.Block _ | Profile _ -> true
    | _ -> false in
  let is_write = function Accesses.Write | Increment -> true | Read -> false in
  let is_read = function Accesses.Read | Increment -> true | Write -> false in
  (* the access kinds of the source and of the destination that form a
     dependence of each kind *)
  let picks = function
    | Flow -> (is_write, is_read)
    | Anti -> (is_read, is_write)
    | Output -> (is_write, is_write)
    | Effects -> ((fun _ -> false), fun _ -> false) in
  let edge_of_pairs ~src ~dst var kind =
    let picks_src, picks_dst = picks kind in
    let distances =
      List.concat_map (accesses_of src var)
        ~f:(fun (src_access : Accesses.access) ->
          List.filter_map (accesses_of dst var)
            ~f:(fun (dst_access : Accesses.access) ->
              match (src_access.kind, dst_access.kind) with
              | Increment, Increment -> None
              | src_kind, dst_kind when picks_src src_kind && picks_dst dst_kind
                ->
                  distance_at_loop ~common_loop_ids ~is_src_first:(src < dst)
                    src_access.path dst_access.path
              | _ -> None)) in
    match distances with
    | [] -> None
    | first :: rest ->
        let distance =
          if List.for_all rest ~f:(fun distance -> distance = first) then first
          else None in
        Some {src; dst; var= Some var; kind; distance} in
  let edges_into dst =
    let dst_accesses = List.assoc dst accesses in
    let vars =
      Set.Poly.to_list
        (Set.Poly.union
           (Accesses.used_vars dst_accesses)
           (Accesses.defined_vars dst_accesses)) in
    List.concat_map vars ~f:(fun var ->
        let writers = reaching_writers dst var in
        List.concat_map [Flow; Output; Anti] ~f:(fun kind ->
            List.filter_map leaves ~f:(fun src ->
                let is_source =
                  match kind with
                  | Flow -> List.mem src ~set:writers
                  | Anti -> src <> dst || compound src
                  | Output | Effects -> true in
                if is_source then edge_of_pairs ~src ~dst var kind else None)))
  in
  let effectful = List.filter leaves ~f:(has_effects statements) in
  let effect_edges =
    List.concat_map effectful ~f:(fun src ->
        List.filter_map effectful ~f:(fun dst ->
            Option.some_if (src <> dst)
              {src; dst; var= None; kind= Effects; distance= None})) in
  let key (edge : edge) = (edge.src, edge.dst, edge.var, edge.kind) in
  { leaves
  ; edges=
      List.sort
        (List.concat_map leaves ~f:edges_into @ effect_edges)
        ~cmp:(fun left right -> compare (key left) (key right))
  ; read_vars=
      Set.Poly.union_list
        (List.map accesses ~f:(fun (_, leaf_accesses) ->
             Accesses.used_vars leaf_accesses)) }

(** The pi-blocks of [graph] (Allen and Kennedy 1987, section 5.2): the strongly
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

(** [S0]: the leaf's number in the report, by position in the body. *)
let pp_position (graph : loop_graph) ppf (label : label) =
  Fmt.pf ppf "S%d"
    (Option.value ~default:(-1)
       (List.find_index graph.leaves ~f:(fun leaf -> leaf = label)))

(** [S0 -> S1 muj d=0 (flow)], [S0 -> S0 v d=1 (flow)], [d=*] for an unknown
    distance, or [S0 -> S2 (effects)]. *)
let pp_edge (graph : loop_graph) ppf (edge : edge) =
  Fmt.pf ppf "%a -> %a" (pp_position graph) edge.src (pp_position graph)
    edge.dst;
  match edge.var with
  | None -> Fmt.string ppf " (effects)"
  | Some var ->
      Fmt.pf ppf " %s d=%s (%s)" var
        (Option.value_map edge.distance ~default:"*" ~f:Int.to_string)
        (match edge.kind with
        | Flow -> "flow"
        | Anti -> "anti"
        | Output -> "output"
        | Effects -> "effects")

(** The statement at [label] on one line and truncated, so a leaf reads as one
    row. *)
let pp_leaf (statements : stmt_info LabelMap.t) ppf (label : label) =
  let unbroken =
    Format.asprintf "%t" (fun ppf ->
        Format.pp_set_margin ppf 100_000;
        Stmt.Located.pp ppf (statement_at statements label)) in
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
let pp_graph (statements : stmt_info LabelMap.t)
    (outcomes : (label * string) list) ppf (graph : loop_graph) =
  List.iteri graph.leaves ~f:(fun number leaf ->
      Fmt.pf ppf "  S%d  %a%a@." number (pp_leaf statements) leaf
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
