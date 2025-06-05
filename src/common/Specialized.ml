(** This module defines signature and functors used to 'specialize' a
fixed-point type that is polymorphic in the type of meta-data to a particular
type of meta-data.

This specialization is useful since we end up with a concrete type of kind [*]
(i.e. not a type _constructor_) allowing us to make the type [Comparable]. In
the analysis and optimization code, we work with map's and sets of our IR types
a lot and this approach makes the types much nicer to work with.
*)

open Core

(** Signature of all meta data used to annotate IRs *)
module type Meta = sig
  type t [@@deriving compare, sexp, hash]

  val empty : t
end

module type Unspecialized = sig
  type 'a t [@@deriving compare, hash, sexp]
end

module type Unspecialized2 = sig
  type ('a, 'b) t [@@deriving compare, hash, sexp]
end

(** Since the type [t] is now concrete (i.e. not a type _constructor_) we can
construct a [Comparable.S] giving us [Map] and [Set] specialized to the type.
*)
module type S = sig
  type t [@@deriving compare, hash, sexp]

  module Meta : Meta
  include Comparator.S with type t := t

  include
    Comparable.S
      with type t := t
       and type comparator_witness := comparator_witness
end

module Make (X : Unspecialized) (Meta : Meta) :
  S with type t = (Meta.t[@compare.ignore]) X.t and module Meta := Meta = struct
  module Basic = struct
    type t = (Meta.t[@compare.ignore]) X.t [@@deriving hash, sexp, compare]

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end

module Make2 (X : Unspecialized2) (First : S) (Meta : Meta) :
  S
    with type t =
      ((First.Meta.t[@compare.ignore]), (Meta.t[@compare.ignore])) X.t
     and module Meta := Meta = struct
  module Basic = struct
    type t = ((First.Meta.t[@compare.ignore]), (Meta.t[@compare.ignore])) X.t
    [@@deriving hash, sexp, compare]

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end
