open Std
open Middle
open Dataflow_types

module Loop_context : sig
  type t

  val create :
       ((Expr.Typed.t, label) Stmt.Pattern.t * 'meta) LabelMap.t
    -> string Set.Poly.t
    -> t

  type scope

  val create_scope : t -> label list -> scope
  val is_loop_variable : scope -> string -> bool
end

module Accesses : sig
  type 'scalar step = Subscript of 'scalar Index.t | Field of int
  type 'scalar location = {variable: string; path: 'scalar step list}

  (** The locations read and written by a statement, including its index
      expressions. A read-write effect occurs in both lists. These lists
      summarize effects; they do not specify evaluation order. *)
  type 'scalar t = {reads: 'scalar location list; writes: 'scalar location list}

  val of_statement :
    (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> Expr.Typed.t t

  val map_scalars : ('scalar -> 'mapped) -> 'scalar t -> 'mapped t
  val read_variables : 'scalar t -> string Set.Poly.t
  val written_variables : 'scalar t -> string Set.Poly.t
end

module Access_function : sig
  type t

  val of_expr : Loop_context.scope -> Expr.Typed.t -> t
end

type path = Access_function.t Accesses.step list

val compare_locations :
     Access_function.t Accesses.location
  -> Access_function.t Accesses.location
  -> int
(** Syntactic location ordering, ignoring expression metadata and cached views.
    This is identity for deduplication, not a proof of equal runtime addresses.
*)

val definitely_overwrites : is_same_iteration:bool -> path -> path -> bool
(** Whether the first path covers the entire second path. The caller establishes
    that the write executes; this query only proves location coverage. *)

type loop_constraint
type dependence_vector = (label * loop_constraint) list

val analyze_dependence :
  label list -> path -> label list -> path -> dependence_vector option
(** Both loop lists describe lexical ancestry in the same region, outermost
    first. Only their common prefix constrains relative iteration order. [None]
    proves independence; [Some []] permits overlap without common-loop ordering
    constraints. *)

val may_write_precede_read : bool -> dependence_vector -> bool
(** Whether a permitted overlap can place the write before the read. The boolean
    says whether the writer precedes the reader within the same iteration. *)
