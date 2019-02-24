open Core_kernel
open Mir
open Dataflow_types

(**
   ~~~~~ STILL TODO ~~~~~
 * Indexed variables are currently handled as monoliths
 * 
   * target terms shouldn't introduce dependency to data variables
   * data-independent target terms might be useful
 * Variables declared in blocks should go out of scope
   * This is done already for for-loop index variables
 * Traverse functions that end in st, since they can change the target
 **)

val program_df_graphs : stmt_loc prog -> prog_df_graphs
(**
   Construct dataflow graphs for each interesting block in the program MIR
*)

val block_dataflow_graph : stmt_loc -> vexpr Set.Poly.t -> dataflow_graph
(**
   Construct a dataflow graph for the block, given the top-level variables
*)

val label_dependencies : dataflow_graph -> bool -> label -> label Set.Poly.t
(**
   Find the set of labels for nodes that could affect the value or behavior of the node
   with `label`.

   If the bool `probabilistic_dependence` is false, the nodes corresponding to target
   terms will not be traversed (recursively), and the result will be the same as a
   classical dataflow analysis.
*)

val labels_dependencies :
  dataflow_graph -> bool -> label Set.Poly.t -> label Set.Poly.t
(**
   Find the set of labels for nodes that could affect the value or behavior of any of the
   nodes `labels`.

   If the bool `probabilistic_dependence` is false, the nodes corresponding to target
   terms will not be traversed (recursively), and the result will be the same as a
   classical dataflow analysis.
*)

val top_var_dependencies :
  dataflow_graph -> label Set.Poly.t -> vexpr Set.Poly.t
(**
   Find the set of top variables that are dependencies for the set of nodes
   `labels`.
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
   Find the set of target term nodes which do not depend on any top variables in
   `exprs`. Only non-probabilistic dependence is considered, otherwise all overlapping terms
   will depend on eachother.
*)

val analysis_example : stmt_loc prog -> string -> unit
(**
   Builds a dataflow graph from the model block and evaluates the label and global
   variable dependencies of the "y" variable, printing results to stdout.
*)
