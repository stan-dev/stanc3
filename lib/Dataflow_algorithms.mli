open Core_kernel
open Mir
open Dataflow_types

(**
   Construct a dataflow graph for the block, given some top (global?) variables
*)
val block_dataflow_graph : stmt_loc -> vexpr Set.Poly.t -> dataflow_graph

(**
   Build the dataflow graphs for each interesting block in the program MIR
*)
val program_df_graphs : stmt_loc prog -> prog_df_graphs

(**
   Find the set of target term nodes which do not depend on any top variables in
   `exprs`. Only non-statistical dependence is considered, otherwise all overlapping terms
   will depend on eachother.
*)
val exprset_independent_target_terms : dataflow_graph -> vexpr Set.Poly.t -> label Set.Poly.t

(**
   Find the set of top variables that are dependencies for the set of nodes
   `labels`.
*)
val top_var_dependencies : dataflow_graph -> label Set.Poly.t -> vexpr Set.Poly.t

(**
   Find the set of labels for nodes that could affect the final value of the variable.

   If `statistical_dependence` is off, the nodes corresponding to target terms will not be
   traversed (recursively), and the result will be the same as a classical dataflow
   analysis.
*)
val final_var_dependencies : dataflow_graph -> bool -> vexpr -> label Set.Poly.t

(**
   Find the set of labels for nodes that could affect the value or behavior of the node
   with `label`.

   If `statistical_dependence` is off, the nodes corresponding to target terms will not be
   traversed (recursively), and the result will be the same as a classical dataflow
   analysis.
*)
val label_dependencies : dataflow_graph -> bool -> label Set.Poly.t -> label -> label Set.Poly.t

(**
   Find the set of labels for nodes that could affect the value or behavior of any of the
   nodes `labels`.

   If `statistical_dependence` is off, the nodes corresponding to target terms will not be
   traversed (recursively), and the result will be the same as a classical dataflow
   analysis.
*)
val labels_dependencies : dataflow_graph -> bool -> label Set.Poly.t -> label Set.Poly.t -> label Set.Poly.t

(**
   Builds a dataflow graph from the model block and evaluates the label and global
   variable dependencies of the "y" variable, printing results to stdout.
*)
val analysis_example : stmt_loc prog -> dataflow_graph
