open Core
open Middle
open Dataflow_types

val union_maps_left :
  ('a, 'b) Map.Poly.t -> ('a, 'b) Map.Poly.t -> ('a, 'b) Map.Poly.t
(** Union maps, preserving the left element in a collision *)

val build_cf_graphs :
     ?flatten_loops:bool
  -> ?blocks_after_body:bool
  -> (label, (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * 'm) Map.Poly.t
  -> label Set.Poly.t
     * (label, label Set.Poly.t) Map.Poly.t
     * (label, label Set.Poly.t) Map.Poly.t
(**
   Simultaneously builds the controlflow parent graph, the predecessor graph and the exit
   set of a statement. It's advantageous to build them together because they both rely on
   some of the same Break, Continue and Return bookkeeping.

   Takes a statement map and returns the triple:
     (exit set, predecessor graph, controlflow parent graph)
   where
     * (exit set, predecessor graph) is the return value of build_predecessor_graph
     * (controlflow parent graph) is the return value of build_cf_graph
*)

val build_cf_graph :
     (label, (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * 'm) Map.Poly.t
  -> (label, label Set.Poly.t) Map.Poly.t
(**
   Building the controlflow graph requires a traversal with state that includes continues,
   breaks, returns and the controlflow graph accumulator. The traversal should be a
   branching traversal with set unions rather than a forward traversal because continue
   and return statements shouldn't affect other branches of execution.
*)

val build_predecessor_graph :
     ?flatten_loops:bool
  -> ?blocks_after_body:bool
  -> (label, (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * 'm) Map.Poly.t
  -> label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t
(**
   Building the predecessor graph requires a traversal with state that includes the
   current previous nodes and the predecessor graph accumulator. Special cases are made
   for loops, because they should include the body exits as predecessors to the body, and
   they should include loop predecessors in their exit sets. I'm not sure if the single
   re-traversal of the loop body is sufficient or this requires finding a fixed-point.
*)

val build_recursive_statement :
     (('e, 's) Stmt.Fixed.Pattern.t -> 'm -> 's)
  -> (label, ('e, label) Stmt.Fixed.Pattern.t * 'm) Map.Poly.t
  -> label
  -> 's
(**
   Build a fixed-point data type representation of a statement given a label-map
   representation.
*)

val is_ctrl_flow : ('a, 'b) Stmt.Fixed.Pattern.t -> bool
(** Check if the statement controls the execution of its substatements. *)

val merge_set_maps :
     ('a, 'b Set.Poly.t) Map.Poly.t
  -> ('a, 'b Set.Poly.t) Map.Poly.t
  -> ('a, 'b Set.Poly.t) Map.Poly.t
(**
   Merge two maps whose values are sets, and union the sets when there's a collision.
*)

val generate_map : 'a Set.Poly.t -> f:('a -> 'b) -> ('a, 'b) Map.Poly.t
(**
   Generate a Map by applying a function to each element of a key set.
*)

val build_statement_map :
     ('s -> ('e, 's) Stmt.Fixed.Pattern.t)
  -> ('s -> 'm)
  -> 's
  -> (label, ('e, label) Stmt.Fixed.Pattern.t * 'm) Map.Poly.t
(**
   The statement map is built by traversing substatements recursively to replace
   substatements with their labels while building up the substatements' statement maps.
   Then, the result is the union of the substatement maps with this statement's singleton
   pair, which is expressed in terms of the new label-containing statement.
*)
