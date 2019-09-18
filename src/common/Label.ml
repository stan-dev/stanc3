open Core_kernel

(* Comparable, pretty-printable labels used to annotate intermediate 
representations*)
module type S = sig
  type t [@@deriving compare, hash, sexp]

  include Pretty.S with type t := t
  include Comparator.S with type t := t

  include
    Comparable.S
    with type t := t
     and type comparator_witness := comparator_witness

  val init : t
  val next : t -> t
end
