open Core

(***********************************)
(* Basic datatypes                 *)
(***********************************)

(**
   A label is a unique identifier for a node in the dataflow/dependency graph, and
   often corresponds to one node in the Mir.
*)
type label = int [@@deriving sexp]

(**
   Representation of an expression that can be assigned to. This should also be able to
   represent indexed variables, but we don't support that yet.
*)
type vexpr = VVar of string [@@deriving sexp]

(**
   A 'reaching definition' (or reaching_defn or RD) statement (v, l) says that the variable
   v could have been affected at the label l.
*)
type reaching_defn = vexpr * label [@@deriving sexp]

(** The most recently nested control flow (block start, if/then, or loop)

    This isn't included in the traversal_state because it only flows downward through the
    tree, not across and up like everything else
*)
type cf_state = label
