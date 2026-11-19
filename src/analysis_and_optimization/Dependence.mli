open Std
open Middle

type access_step = Subscript of Expr.Typed.t Index.t | TupleField of int

(** Selections in order from the base variable. The empty path selects the whole
    variable. For [t.2[i]] the path is [[TupleField 2; Subscript (Single i)]].
*)
type access_path = access_step list

type location = {variable: string; path: access_path}
type accesses = {reads: location list; writes: location list}

val pp_access_path : Expr.Typed.t Fmt.t -> access_path Fmt.t

module Location : sig
  val same_var : location -> location -> bool
  (** Compare the base variable names of two locations. *)
end

val collect_accesses : (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> accesses
(** Includes reads in index expressions; reads and writes are unordered. *)

val may_overlap : access_path -> access_path -> bool
(** [may_overlap p q] checks whether two reads or writes to the same variable
    could select a common element. For example, [v[1:2]] and [v[2:3]] share
    element 2. For [v[i]] and [v[j]], the result is also true when the analysis
    cannot rule out [i = j]. *)

val is_overwritten : string Set.Poly.t -> location -> location -> bool
(** [is_overwritten written_vars previous write] proves that [write] covers
    [previous]. False means the previous definition must be retained.
    [written_vars] must include every variable written in the analyzed region.
*)
