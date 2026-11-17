open Std
open Middle

type step = Subscript of Expr.Typed.t Index.t | Field of int
type path = step list
type location = {variable: string; path: path}
type accesses = {reads: location list; writes: location list}

val collect_accesses : (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> accesses
(** Includes reads in index expressions; reads and writes are unordered. *)

val may_overlap : path -> path -> bool
(** False proves disjoint paths. Unknown indices and slices permit overlap. *)

val definitely_overwrites : string Set.Poly.t -> path -> path -> bool
(** The variable set must include every variable written in the analyzed region.
*)
