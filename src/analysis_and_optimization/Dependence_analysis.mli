open Std
open Middle
open Dataflow_types

(** Which statements of a block depend on which other statements.

    A statement depends on the [if] or loop that decides whether the statement
    runs, and on the statements that may have assigned a value the statement
    reads. Pedantic mode uses this to warn when a condition depends on a
    parameter, and the factor graph uses this to find which data and parameters
    each [target] term reads.

    For a read of a variable, the candidate writers are the reaching definitions
    of that variable. The analysis also records which elements each statement
    reads and writes (the {!access} type), and drops a writer that cannot have
    written an element the read uses. With labels on the right:
    {[
      theta[1] = a;          // 5
      theta[2] = 1;          // 6
      if (theta[2] > 0) ...  // 7
    ]}
    Statement 7 reads [theta[2]]. The declaration of [theta] and statements 5
    and 6 all assign [theta] before statement 7, but statement 5 writes only
    [theta[1]]. So statement 7 depends on the declaration and on statement 6,
    and the [if] does not depend on [a]. *)

(** ~~~~~ TODO ~~~~~
    - The interfaces are currently messed up. I think part of the solution is to
      change the signature of reaching_definitions_mfp in Monotone_framework,
      which currently requires the full program but shouldn't need the full
      program. As it stands, stmt_map_dependency_graph does not include data
      dependencies at all, since it can't use reaching deps, and prog_dependency
      graph only builds the graph for log_prob, but the user isn't guaranteed to
      be using the same labeling scheme.
    - Currently, dependencies on global or uninitialized data are written as
      depending on node '0'. This should probably be option or some type that
      indicates global dependence.
    - No probabilistic dependency, I'll do that elsewhere **)

(** {1 Accesses} *)

(** An integer index expression written as a constant, plus at most one
    loop-invariant expression, plus at most one loop variable. In a loop over
    [n], with [k] a data variable:
    - [3] is [{const= 3; symbol= None; loopvar= None}]
    - [k + 1] is [{const= 1; symbol= Some k; loopvar= None}]
    - [n + k - 1] is [{const= -1; symbol= Some k; loopvar= Some "n"}]

    [symbol] stands for an expression whose value is the same in every
    iteration. Two symbols are treated as equal only when the two expressions
    are identical. *)
type linear = {const: int; symbol: Expr.Typed.t option; loopvar: string option}

(** Why an index expression is not a [linear]. *)
type varying_kind =
  | Written
      (** The index reads a variable that is assigned inside the analysed
          statement, so the index value can change between iterations. *)
  | Nonlinear
      (** The index uses a loop variable in some other way, for example
          [idx[n]], [2 * n], [n + m] or [N - n]. *)

(** One single index of an access, such as the [n + 1] in [x[n + 1]]. Two
    [Affine] indices can be compared (the ZIV and SIV tests of Goff, Kennedy and
    Tseng 1991, section 3). *)
type point =
  | Affine of linear
      (** The index is a [linear]. With [loopvar = None] the index has the same
          value in every iteration. *)
  | Varying of varying_kind
      (** The index cannot be compared with another index, so the analysis
          assumes the two indices can be equal. *)

(** How a statement uses a variable. An [Increment], such as [target += ...],
    reads and writes the variable, but two increments of one variable can run in
    either order. *)
type access_kind = Read | Write | Increment

(** One read or write of a variable by a statement. [subs] holds one entry per
    index position, so [x[i, n + 1]] has two entries. A use of the whole
    variable, such as [v] or the declaration of [v], has no entries. *)
type access = {var: string; subs: point Index.t list; kind: access_kind}

(** {1 Dependences between two accesses} *)

(** For two accesses inside one loop, how the iteration of the first access
    compares with the iteration of the second: [Lt] means the first access runs
    in an earlier iteration, [Eq] in the same iteration, [Gt] in a later one. *)
type direction = Lt | Eq | Gt

(** What is known about one loop for two accesses that may touch the same
    element: the [directions] that are possible and, when known, the [distance],
    the number of iterations from the first access to the second. *)
type level = {directions: direction Set.Poly.t; distance: int option}

(** Whether two accesses can touch the same element. [Dependent levels] has one
    [level] per loop around both accesses, outermost first; this is the
    direction vector of Allen and Kennedy (1987). In
    {[
      for (n in 2:N) a[n] = a[n - 1];
    ]}
    the write in iteration [n - 1] and the read in iteration [n] touch the same
    element, so the dependence from the write to the read is
    [Dependent [{directions= {Lt}; distance= Some 1}]]. *)
type dependence = Independent | Dependent of level list

(** {1 The dependency information} *)

(** What the analysis records about one statement. *)
type node_dep_info =
  { predecessors: label Set.Poly.t
        (** the statements that can run just before this one *)
  ; parents: label Set.Poly.t
        (** the [if] and loop statements that decide whether this statement runs
        *)
  ; reaching_defn_entry: reaching_defn Set.Poly.t
        (** the assignments that may reach the start of this statement *)
  ; reaching_defn_exit: reaching_defn Set.Poly.t
        (** the assignments that may reach the end of this statement *)
  ; loop: label option
        (** the innermost [for] or [while] loop around this statement *)
  ; accesses: access list
        (** the reads and writes of this statement, not counting the statements
            nested inside *)
  ; meta: Location_span.t  (** the source location *) }

(** Every statement inside the analysed statement, by label, with the
    statement's children replaced by the children's labels. *)
type dep_info_map =
  ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t

(** For each label, every label the statement depends on, directly or through
    other statements. *)
type dependency_graph = label Set.Poly.t LabelMap.t

val classify_point :
     loopvars:string Set.Poly.t
  -> written_vars:string Set.Poly.t
  -> Expr.Typed.t
  -> point
(** The integer index [expr] as a [point]. [loopvars] are the variables of the
    loops around the index, and [written_vars] the variables assigned in the
    analysed statement. *)

val node_immediate_dependencies :
  dep_info_map -> ?blockers:string Set.Poly.t -> label -> label Set.Poly.t
(** Given dependency information for each node, find the 'immediate'
    dependencies of a node: the [if] and loop statements around the node, and
    the assignments that may have written an element the node reads before the
    node runs. *)

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
(** Build the dependency information for each statement inside [stmt]. *)

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

val rhs_variables_at : dep_info_map -> label Set.Poly.t -> string Set.Poly.t
(** The variables read by the statements at [labels], including the variables
    read inside indices. *)
