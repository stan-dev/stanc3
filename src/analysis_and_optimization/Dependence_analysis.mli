open Std
open Middle
open Dataflow_types

(** Dependence analysis, where a statement depends on the [if] and loops around
    the statement and on the assignments to the elements the statement reads. *)

(** ~~~~~ TODO ~~~~~
    - [reaching_definitions_mfp] in Monotone_framework takes the whole program
      but needs only the names of the variables defined before the statement.
    - A variable defined outside the analysed statement, such as data, gets a
      reaching definition at label 1, which is also the label of the analysed
      statement itself, so callers must test for label 1; keeping the label a
      [label option] in [reaching_defn] would separate the two. **)

(** {1 Accesses} *)

(** An integer index of the form [const + symbol + loopvar], where [symbol] is a
    loop-invariant expression, such as [n + k - 1] in a loop over [n]. *)
type linear = {const: int; symbol: Expr.Typed.t option; loopvar: string option}

(** Why an index expression is not a [linear]. *)
type varying_kind =
  | Written
      (** The index reads a variable assigned inside the analysed statement, so
          the index can change between iterations. *)
  | Nonlinear
      (** The index uses a loop variable in some other form, such as [idx[n]] or
          [2 * n]. *)

(** One single index of an access in the form that the ZIV and SIV tests compare
    (Goff, Kennedy and Tseng 1991). *)
type point =
  | Affine of linear
      (** The index is a [linear], which has the same value in every iteration
          when [loopvar] is [None]. *)
  | Varying of varying_kind
      (** The index cannot be compared, so the index may equal any other. *)

(** One index position or tuple field of an access path. *)
type 'index step =
  | Subscript of 'index Index.t  (** One index position, such as [n + 1]. *)
  | Field of int  (** One tuple field, such as the [.2] in [t.2]. *)

(** One use of a variable by a statement, with one [path] step per index
    position and tuple field. *)
type 'index access = {var: string; path: 'index step list}

(** The accesses of one statement, split into reads, writes and increments. *)
module Accesses : sig
  type 'index t =
    { reads: 'index access list
    ; writes: 'index access list
    ; increments: 'index access list
          (** the accesses that read and write, such as [target += ...], but
              commute with each other *) }

  val written_vars : 'index t -> string Set.Poly.t
  (** The names of the variables that the accesses write or increment. *)
end

(** {1 Dependences between two accesses} *)

(** Whether the first of two accesses runs in an earlier ([Lt]), the same ([Eq])
    or a later ([Gt]) iteration of one loop than the second. *)
type direction = Lt | Eq | Gt

(** The possible [directions] for one loop and, when known, the [distance] in
    iterations from the first access to the second. *)
type level = {directions: direction Set.Poly.t; distance: int option}

module Dependence : sig
  (** Whether two accesses can touch the same element. *)
  type t =
    | Independent  (** The accesses never touch the same element. *)
    | Unknown
        (** The accesses cannot be compared, as in [x[idx[n]]] against [x[n]] (a
            confused dependence in LLVM). *)
    | Dependent of level list
        (** The accesses may touch the same element, with one [level] per common
            loop, outermost first (Allen and Kennedy 1987). *)

  (** Which uses a dependence connects, as named by Kuck (1978). *)
  type kind =
    | Flow  (** A write, then a read of the written element. *)
    | Anti  (** A read, then a write of the read element. *)
    | Output  (** Two writes of one element. *)
end

(** {1 The dependency information} *)

(** What the analysis records about one statement. *)
type node_dep_info =
  { predecessors: label Set.Poly.t
        (** the statements that can run just before this one *)
  ; parents: label Set.Poly.t
        (** the [if] and loops that decide whether this statement runs *)
  ; reaching_defn_entry: reaching_defn Set.Poly.t
        (** the assignments that may reach the start of this statement *)
  ; reaching_defn_exit: reaching_defn Set.Poly.t
        (** the assignments that may reach the end of this statement *)
  ; loop: label option
        (** the innermost [for] or [while] loop around this statement *)
  ; accesses: point Accesses.t
        (** the reads and writes of this statement, not of nested statements *)
  ; meta: Location_span.t  (** the source location *) }

(** Every statement inside the analysed statement by label, with children
    replaced by the children's labels. *)
type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

(** Every label each statement depends on, directly or transitively. *)
type dependency_graph = label Set.Poly.t LabelMap.t

val node_immediate_dependencies :
  dep_info_map -> ?blockers:string Set.Poly.t -> label -> label Set.Poly.t
(** The [if] and loop statements around a statement and the assignments that may
    have written an element the statement reads outside [blockers]. *)

val node_dependencies : dep_info_map -> label -> label Set.Poly.t
(** Given dependency information for each node, find all of the dependencies of
    a single node. *)

val node_vars_dependencies :
     dep_info_map
  -> ?blockers:string Set.Poly.t
  -> string Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Given dependency information for each node, find all of the dependencies of
    a set of variables at single node.

    'blockers' are variables which will not be traversed. *)

val build_dep_info_map : Program.Typed.t -> Stmt.Located.t -> dep_info_map
(** The dependency information for each statement inside the given statement. *)

val log_prob_build_dep_info_map : Program.Typed.t -> dep_info_map
(** Build the dependency information for each node in the log_prob section of a
    program *)

val all_node_dependencies : dep_info_map -> dependency_graph
(** Given dependency information for each node, find all of the dependencies of
    all nodes, effectively building the dependency graph.

    This is more efficient than calling node_dependencies on each node
    individually. *)

val log_prob_dependency_graph : Program.Typed.t -> dependency_graph
(** Build the dependency graph for the log_prob section of a program, where
    labels correspond to the labels built by statement_map. *)

val reaching_defn_lookup :
  reaching_defn Set.Poly.t -> string -> label Set.Poly.t
(** The labels where the named variable may have been assigned. *)

val mir_uninitialized_variables :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(** Produce a list of uninitialized variables and their label locations, from
    the flowgraph starting at the given statement *)

val read_variables_at : dep_info_map -> label Set.Poly.t -> string Set.Poly.t
(** The variables that the statements at [labels] read or increment, counting
    only the bounds of a [for] and the condition of an [if]. *)

(** {2 The loop dependence graph of one [For]} *)

(** How the sink depends on the source: reads what the source wrote, writes what
    the source read, writes what the source wrote, or both have effects. *)
type dep_kind = Flow | Anti | Output | Effects

(** [src] executes no later than [dst] in the original loop (Kennedy and Allen
    2001, Definition 2.1). [dep] is the element test between the two leaves'
    accesses to [var] at the analysed loop's level, restricted to that order;
    [var] is [None] for [Effects]. *)
type edge =
  {src: label; dst: label; var: string option; kind: dep_kind; dep: Dependence.t}

(** The graph of one [For]: the leaves of the body in lexical order and the
    edges between them. *)
type loop_graph = {leaves: label list; edges: edge list}

val root_label : label
(** The label of the analysed statement itself, [1]. *)

val subtree_accesses : dep_info_map -> label -> point Accesses.t
(** The accesses of the statement at the label and of every statement below. *)

val statement_at : dep_info_map -> label -> Stmt.Located.t
(** The statement at the label, children rebuilt from the map. *)

val leaf_has_effects : dep_info_map -> label -> bool
(** Whether the leaf, or a statement below, prints, rejects, calls a user
    function as a statement, or evaluates a side-effecting or random expression.
*)

val build_loop_graph : dep_info_map -> loop:label -> loop_graph
(** The flow, anti, output and effects edges between the leaves of the [For] at
    [loop], at that loop's level; the implementation documents the rules. *)

val pi_blocks : loop_graph -> label list list
(** The strongly connected components in emission order (Allen and Kennedy 1987
    §5.2): a topological order of the condensation, ties to the earliest leaf.
*)

val is_cyclic : dep_info_map -> loop_graph -> label list -> bool
(** Whether a pi-block must stay a sequential loop; a write-only scatter's self
    output dependence does not count, since indexed assignment stores in order.
*)

val edge_between : loop_graph -> from:label list -> into:label list -> bool
(** Whether some edge leaves a leaf of [from] for a leaf of [into]. *)

val pp_edge : loop_graph -> Format.formatter -> edge -> unit
(** [S0 -> S1 muj {=} d=0 (flow)] or [S0 -> S2 (effects)]. *)

val pp_graph :
     ?outcome:(label -> string option)
  -> dep_info_map
  -> Format.formatter
  -> loop_graph
  -> unit
(** One indented line per leaf, with the leaf's [outcome] after it when given,
    then the edges and the pi-blocks in emission order. *)
