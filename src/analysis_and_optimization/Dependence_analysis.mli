open Std
open Middle
open Dataflow_types

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
    - Indexed variables are currently handled as monoliths
    - No probabilistic dependency, I'll do that elsewhere **)

(** Sufficient information about each node to build the dependency graph.

    Label dependence doesn't need the exit RD set, but variable dependence does.
*)
type node_dep_info =
  { predecessors: label Set.Poly.t
  ; parents: label Set.Poly.t
  ; reaching_defn_entry: reaching_defn Set.Poly.t
  ; reaching_defn_exit: reaching_defn Set.Poly.t
  ; meta: Location_span.t }

val node_immediate_dependencies :
     ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t
  -> ?blockers:vexpr Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Given dependency information for each node, find the 'immediate'
    dependencies of a node, where 'immediate' means the first-degree control
    flow parents and the reachable definitions of RHS variables. *)

val node_dependencies :
     ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t
  -> label
  -> label Set.Poly.t
(** Given dependency information for each node, find all of the dependencies of
    a single node. *)

val node_vars_dependencies :
     ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t
  -> ?blockers:vexpr Set.Poly.t
  -> vexpr Set.Poly.t
  -> label
  -> label Set.Poly.t
(** Given dependency information for each node, find all of the dependencies of
    a set of variables at single node.

    'blockers' are variables which will not be traversed. *)

val build_dep_info_map :
     Program.Typed.t
  -> Stmt.Located.t
  -> ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t
(** Build the dependency information for each node in the log_prob section of a
    program *)

val log_prob_build_dep_info_map :
     Program.Typed.t
  -> ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t
(** Build the dependency information for each node in the log_prob section of a
    program *)

val all_node_dependencies :
     ((Expr.Typed.t, label) Stmt.Pattern.t * node_dep_info) LabelMap.t
  -> label Set.Poly.t LabelMap.t
(** Given dependency information for each node, find all of the dependencies of
    all nodes, effectively building the dependency graph.

    This is more efficient than calling node_dependencies on each node
    individually. *)

val log_prob_dependency_graph : Program.Typed.t -> label Set.Poly.t LabelMap.t
(** Build the dependency graph for the log_prob section of a program, where
    labels correspond to the labels built by statement_map. *)

val reaching_defn_lookup : reaching_defn Set.Poly.t -> vexpr -> label Set.Poly.t

val mir_uninitialized_variables :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(** Produce a list of uninitialized variables and their label locations, from
    the flowgraph starting at the given statement *)

(** {1 Loop access model and dependence test}

    Data dependence analysis for one loop level (Kennedy and Allen,
    {i Optimizing Compilers for Modern Architectures} ch. 2-3; Goff, Kennedy and
    Tseng, PLDI 1991). See [design-docs/active/vectorize-loop-fission.md]
    §7.3-7.4. The documentation of each function is on its definition. *)

val classify_subscript :
     loopvar:string
  -> written:string Set.Poly.t
  -> Expr.Typed.t Index.t
  -> subscript
(** How one index position varies with the loop over [loopvar]. *)

val stmt_accesses :
     loopvar:string
  -> written:string Set.Poly.t
  -> label:label
  -> (Expr.Typed.t, Stmt.Located.t) Stmt.Pattern.t
  -> access list
(** Every read and write in a statement, in evaluation order. *)

val access_dependence : access -> access -> dependence
(** The dependence between two accesses to the same variable. *)

val pp_subscript : subscript Fmt.t
val pp_access : access Fmt.t
val pp_dependence : dependence Fmt.t
