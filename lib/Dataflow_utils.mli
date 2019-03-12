open Core_kernel
open Mir
open Dataflow_types

val build_cf_graph : (int statement * 'm) Int.Map.t -> Int.Set.t Int.Map.t
(**
   Building the controlflow graph requires a traversal with state that includes continues,
   breaks, returns and the controlflow graph accumulator. The traversal should be a
   branching traversal with set unions rather than a forward traversal because continue
   and return statements shouldn't affect other branches of execution.
*)

val build_predecessor_graph : (int statement * 'm) Int.Map.t -> Int.Set.t * Int.Set.t Int.Map.t
(**
   Building the predecessor graph requires a traversal with state that includes the
   current previous nodes and the predecessor graph accumulator. Special cases are made
   for loops, because they should include the body exits as predecessors to the body, and
   they should include loop predecessors in their exit sets. I'm not sure if the single
   re-traversal of the loop body is sufficient or this requires finding a fixed-point.
*)

val build_recursive_statement : ('s statement -> 'm -> 's) -> ((label statement * 'm) Int.Map.t) -> label -> 's
(**
   Build a fixed-point data type representation of a statement given a label-map
   representation.
*)

val build_statement_map : ('s -> 's statement) -> ('s -> 'm) -> 's -> (int statement * 'm) Int.Map.t
(**
   The statement map is built by traversing substatements recursively to replace
   substatements with their labels while building up the substatements' statement maps.
   Then, the result is the union of the substatement maps with this statement's singleton
   pair, which is expressed in terms of the new label-containing statement.
*)
