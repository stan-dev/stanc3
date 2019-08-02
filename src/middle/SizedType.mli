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

val sint : 'a t
val sreal : 'a t
val svector : 'a -> 'a t
val srowvector : 'a -> 'a t
val smatrix : 'a -> 'a -> 'a t
val sarray : 'a t -> 'a -> 'a t
val to_unsizedtype : 'a t -> UnsizedType.t

val associate :
     ?init:Expr.Labelled.t Label.Map.t
  -> Expr.Labelled.t t
  -> Expr.Labelled.t Label.Map.t
