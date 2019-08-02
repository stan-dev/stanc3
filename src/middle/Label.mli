open Core_kernel
open Common

type t = int [@@deriving compare, hash, sexp]

val init : t
val next : t -> t

include Pretty.S with type t := t
include Comparator.S with type t := t

include
  Comparable.S
  with type t := t
   and type comparator_witness := comparator_witness
