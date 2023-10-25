open Core
open Middle

(***********************************)
(* Basic datatypes                 *)
(***********************************)

(**
   A label is a unique identifier for a node in the dataflow/dependency graph, and
   often corresponds to one node in the Mir.
*)
type label = int [@@deriving sexp, hash, compare]

(**
   Representation of an expression that can be assigned to. This should also be able to
   represent indexed variables, but we don't support that yet.
*)
type vexpr = VVar of string [@@deriving sexp, hash, compare]

(**
   A 'reaching definition' (or reaching_defn or RD) statement (v, l) says that the variable
   v could have been affected at the label l.
*)
type reaching_defn = vexpr * label [@@deriving sexp, hash, compare]

(**
   Description of where a node in the dependency graph came from, where MirNode is the
   location from an Middle.loc_stmt
 *)
type source_loc =
  | MirNode of Location_span.t
  | StartOfBlock
  | TargetTerm of {term: Expr.Typed.t; assignment_label: label}
[@@deriving sexp]

(**
   Information to be collected about each node
   * rd_sets: Information about how the label effects the reaching definition sets
   * possible_previous: The set of nodes that could have immediately preceded this node
     under some execution of the program
   * rhs_set: The 'right hand side' set of variables that affect the value or behavior of
     this node
   * controlflow: The set of control flow nodes that are immediate parents of this node:
     * The most recent nested if/then or loop,
     * or the beginning of the function or block if there are no containing branches,
     * plus the set of relevant continue/return statements,
     * plus, for loops, any break statements they contain
   * loc: The location of the Mir node that this node corresponds to, or a description if
     there is none
*)
type 'rd_info node_info =
  { rd_sets: 'rd_info
  ; possible_previous: label Set.Poly.t
  ; rhs_set: vexpr Set.Poly.t
  ; controlflow: label Set.Poly.t
  ; loc: source_loc }
[@@deriving sexp]

(**
   A node_info, where the reaching definition information takes the form of an update
   function that maps from the 'entry' set to the 'exit' set, where the entry set is
   what's true before executing this node and the exit set is true after.
*)
type node_info_update =
  (reaching_defn Set.Poly.t -> reaching_defn Set.Poly.t) node_info

(**
   A node_info where the reaching definition information is explicitly written as the
   entry and exit sets, as after finding the fixed-point solution.
*)
type node_info_fixedpoint =
  (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) node_info
[@@deriving sexp]

(**
   The state that will be maintained throughout the traversal of the Mir
   * label_ix: The next label that's free to use
   * node_info_map: The label information that's been built so far
   * possible_previous: The set of nodes that could have immediately preceded this point
     under some execution of the program
   * target_terms: The set of nodes that correspond to terms added to the target variable
   * continues: A set of the continue nodes that have been encountered since exiting a loop
   * breaks: A set of the break nodes that have been encountered since exiting a loop
   * returns: A set of the return nodes that have been encountered
*)
type traversal_state =
  { label_ix: label
  ; node_info_map: (int, node_info_update) Map.Poly.t
  ; possible_previous: label Set.Poly.t
  ; target_terms: label Set.Poly.t
  ; continues: label Set.Poly.t
  ; breaks: label Set.Poly.t
  ; returns: label Set.Poly.t
  ; rejects: label Set.Poly.t }

(** The most recently nested control flow (block start, if/then, or loop)

    This isn't included in the traversal_state because it only flows downward through the
    tree, not across and up like everything else
*)
type cf_state = label

(**
   Everything we need to know to do dependency analysis
   * node_info_map: Collection of node information
   * possible_exits: Set of nodes that could be the last to execute under some execution
   * probabilistic_nodes: Set of nodes corresponding to which can only introduce
     probabilistic dependencies, such as target terms and reject statements, to be
     excluded for non-statistical dependency analysis
*)
type dataflow_graph =
  { node_info_map: (int, node_info_fixedpoint) Map.Poly.t
  ; possible_exits: label Set.Poly.t
  ; probabilistic_nodes: label Set.Poly.t }
[@@deriving sexp]

(**
   Represents the dataflow graphs for each interesting block in the program MIR.

   See Middle.prog for block descriptions.
*)
type prog_df_graphs =
  {tdatab: dataflow_graph; modelb: dataflow_graph; gqb: dataflow_graph}
[@@deriving sexp]
