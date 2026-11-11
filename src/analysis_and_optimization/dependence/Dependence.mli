open Std
open Middle
open Dataflow_types

module Loop_context : sig
  type t

  val create :
       ((Expr.Typed.t, label) Stmt.Pattern.t * 'meta) LabelMap.t
    -> defined_vars:string Set.Poly.t
    -> t

  val for_loop_id : t -> loop_ids:label list -> string -> label option
end

module Accesses : sig
  type 'scalar step = Subscript of 'scalar Index.t | Field of int
  type kind = Read | Write | ReadWrite [@@deriving equal]
  type 'scalar access = {var: string; path: 'scalar step list; kind: kind}
  type 'scalar t = 'scalar access list

  val of_statement :
    (Expr.Typed.t, 'substatement) Stmt.Pattern.t -> Expr.Typed.t t

  val map_scalars : f:('scalar -> 'mapped) -> 'scalar access -> 'mapped access
  val used_vars : 'scalar t -> string Set.Poly.t
  val defined_vars : 'scalar t -> string Set.Poly.t
  val pp_path : 'scalar Fmt.t -> 'scalar step list Fmt.t
  val pp_access : 'scalar Fmt.t -> 'scalar access Fmt.t
  val pp : 'scalar Fmt.t -> 'scalar t Fmt.t
end

module Access_function : sig
  type t

  val of_expr : Loop_context.t -> loop_ids:label list -> Expr.Typed.t -> t
  val pp : loop_name:(label -> string) -> t Fmt.t
end

type path = Access_function.t Accesses.step list

val is_overwritten : is_same_iteration:bool -> path -> path -> bool

type t

val test_dependence : label list * path -> label list * path -> t
val is_overlap_possible : t -> bool
val is_writer_possibly_first : is_writer_first:bool -> t -> bool
val known_distance : loop_id:label -> t -> int option
val pp : loop_name:(label -> string) -> t Fmt.t
