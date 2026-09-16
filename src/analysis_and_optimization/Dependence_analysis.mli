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
    - No probabilistic dependency, I'll do that elsewhere **)

(** What the analysis knows about one statement; the overview and a worked
    example are at the top of the implementation. *)
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

val node_immediate_dependencies :
  dep_info_map -> ?blockers:string Set.Poly.t -> label -> label Set.Poly.t
(** Given dependency information for each node, find the 'immediate'
    dependencies of a node: the first-degree control flow parents and the
    subscript-pruned reaching definitions of RHS variables. *)

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
(** Build the dependency information for each node in a statement of a program.
*)

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
(** The labels at which the named variable may have been defined. *)

val mir_uninitialized_variables :
  Program.Typed.t -> (Location_span.t * string) Set.Poly.t
(** Produce a list of uninitialized variables and their label locations, from
    the flowgraph starting at the given statement *)

val access_dependence : access -> access -> dependence
(** Whether two accesses to the same variable can name the same element, and in
    which iterations (Goff, Kennedy and Tseng, PLDI 1991). *)

val rhs_variables_at : dep_info_map -> label Set.Poly.t -> string Set.Poly.t
(** The right-hand-side variables of the statements at [labels]. *)

val pp_access : access Fmt.t
val pp_dependence : dependence Fmt.t
val pp_dependency_graph : dependency_graph Fmt.t
val pp_node_accesses : dep_info_map Fmt.t
