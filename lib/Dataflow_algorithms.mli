open Core_kernel
open Mir
open Dataflow_types

(**
   ~~~~~ Still to implement: TODO ~~~~~
 * Indexed variables are currently handled as monoliths
 * Dependency functions with `probabilistic_dependence` set to true:
   * Probably don't correctly traverse the probabilistic graphical model
   * Shouldn't introduce dependency to non-parameter variables
 * Variables declared in blocks should go out of scope
   * This is done already for for-loop index variables
 * Traverse functions that end in st, since they can change the target
 **)

(**
   ~~~~ Untested/doesn't work: ~~~~~

   * Any function that takes a `probabilistic_dependence` bool has untested behavior when that bool is true. I haven't fully thought out the probabilistic dependency (factor) graph, and I will probably end up re-implementing it.
   * The top_var_dependencies function needs reworking
   * The exprset_independent_target_terms might be outdated with TargetPE now in the MIR
 **)

val program_df_graphs : stmt_loc prog -> prog_df_graphs
(**
   Construct dataflow graphs for each interesting block in the program MIR

   For example usage and output, see the expect test "program_df_graphs example" in
   Dataflow_algorithms.ml
*)

val block_dataflow_graph : stmt_loc -> vexpr Set.Poly.t -> dataflow_graph
(**
   Construct a dataflow graph for the block, given the top-level variables

   For example usage and output, see the expect test "block_dataflow_graph example" in
   Dataflow_algorithms.ml
*)

val label_dependencies : dataflow_graph -> bool -> label -> label Set.Poly.t
(**
   Find the set of labels for nodes that could affect the value or behavior of the node
   with the label.

   If the bool `probabilistic_dependence` is false, the nodes corresponding to target
   terms will not be traversed (recursively), and the result will be the same as a
   classical dataflow analysis.

   `label_dependences df_graph prob_dep label` is equivalent to
   `label_dependences df_graph prob_dep (Set.Poly.singleton label)`
*)

val labels_dependencies :
  dataflow_graph -> bool -> label Set.Poly.t -> label Set.Poly.t
(**
   Find the set of labels for nodes that could affect the value or behavior of any of the
   nodes in the label set.

   If the bool `probabilistic_dependence` is false, the nodes corresponding to target
   terms will not be traversed (recursively), and the result will be the same as a
   classical dataflow analysis.

   For example usage and output, see the expect test "labels_dependencies example" in
   Dataflow_algorithms.ml
*)

val top_var_dependencies :
  dataflow_graph -> label Set.Poly.t -> vexpr Set.Poly.t
(**
   ~~~ TODO: untested ~~~

   Find the set of top variables that are dependencies for the given set of nodes
*)

val final_var_dependencies :
  dataflow_graph -> bool -> vexpr -> label Set.Poly.t
(**
   Find the set of labels for nodes that could affect the final value of the variable.

   If the bool `probabilistic_dependence` is false, the nodes corresponding to target
   terms will not be traversed (recursively), and the result will be the same as a
   classical dataflow analysis.
*)

val exprset_independent_target_terms :
  dataflow_graph -> vexpr Set.Poly.t -> label Set.Poly.t
(**
   ~~~ TODO: Possibly outdated with new MIR ~~~

   Find the set of target term nodes which do not depend on any top variables in
   `exprs`. Only non-probabilistic dependence is considered, otherwise all overlapping terms
   will depend on eachother.
*)

val analysis_example : stmt_loc prog -> string -> unit
(**
   Builds a dataflow graph from the model block and evaluates the label and global
   variable dependencies of the "y" variable, printing results to stdout.
*)
