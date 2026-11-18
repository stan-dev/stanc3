open Std
open Middle

(** Tuple fields use [Single] integer literals. Compare paths only within the
    same base variable. *)
type path = Expr.Typed.t Index.t list

type location = {variable: string; path: path}
type accesses = {reads: location list; writes: location list}

val collect_accesses : (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> accesses
(** Includes reads in index expressions; reads and writes are unordered. *)

val may_overlap : path -> path -> bool
(** False proves disjoint paths. Unknown indices and slices permit overlap. *)

val is_overwritten : string Set.Poly.t -> location -> location -> bool
(** [is_overwritten defined_vars previous write] proves that [write] covers
    [previous]. False means the previous definition must be retained.
    [defined_vars] must include every variable written in the analyzed region.
*)
