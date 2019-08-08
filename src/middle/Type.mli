open Core_kernel
open Common

type 'a t = 'a Mir_pattern.possiblysizedtype =
  | Sized of 'a SizedType.t
  | Unsized of UnsizedType.t
[@@deriving sexp, compare, map, hash, fold]

include Pretty.S1 with type 'a t := 'a t

module Make_traversable (A : Applicative.S) :
  Traversable.S with module A := A and type 'a t := 'a t

module Make_traversable2 (A : Applicative.S2) :
  Traversable.S2 with module A := A and type 'a t := 'a t

val to_unsizedtype : 'a t -> UnsizedType.t
val collect_exprs : 'a t -> 'a list

val associate :
     ?init:Expr.Labelled.t Int_label.Map.t
  -> Expr.Labelled.t t
  -> Expr.Labelled.t Int_label.Map.t
