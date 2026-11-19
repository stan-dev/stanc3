open Std
open Middle

(** The steps from a variable to the part being read or written. An empty path
    selects the whole variable. For [t.2[i]], the steps are [Single 2] for the
    tuple field followed by [Single i] for the subscript. *)
type access_path = Expr.Typed.t Index.t list

type location = {variable: string; path: access_path}
type accesses = {reads: location list; writes: location list}

module Location : sig
  val same_var : location -> location -> bool
  (** Compare the base variable names of two locations. *)
end

val collect_accesses : (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> accesses
(** Includes reads in index expressions; reads and writes are unordered. *)

val may_overlap : access_path -> access_path -> bool
(** False proves disjoint paths. Compares literal indices, range bounds and
    index-array elements, and integer parity; unknown values permit overlap. *)

val is_overwritten : string Set.Poly.t -> location -> location -> bool
(** [is_overwritten written_vars previous write] proves that [write] covers
    [previous]. False means the previous definition must be retained.
    [written_vars] must include every variable written in the analyzed region.
*)
