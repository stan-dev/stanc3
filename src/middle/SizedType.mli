open Core_kernel
open Common

type 'a t = 'a Mir_pattern.sizedtype =
  | SInt
  | SReal
  | SVector of 'a
  | SRowVector of 'a
  | SMatrix of 'a * 'a
  | SArray of 'a t * 'a
[@@deriving sexp, compare, map, hash, fold]

include Pretty.S1 with type 'a t := 'a t

module Make_traversable (A : Applicative.S) :
  Traversable.S with module A := A and type 'a t := 'a t

module Make_traversable2 (A : Applicative.S2) :
  Traversable.S2 with module A := A and type 'a t := 'a t

val remove_size : 'a t -> UnsizedType.t
