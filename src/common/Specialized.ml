open Core_kernel

(* Signature of all meta data used to annotate IRs *)
module type Meta = sig
  type t [@@deriving compare, sexp, hash]

  include Pretty.S with type t := t
end

(* Signature of modules derived from `Fixed.S` or `Fixed.S2` with a module,
with a signature matching `Meta`, providing a specific type of meta-data.

Since the type `t` is now concrete (i.e. not a type _constructor_) we can
construct a `Comparable.S` giving us `Map` and `Set` specialized to the type.
*)
module type S = sig
  type t [@@deriving compare, sexp]

  module Meta : Meta
  include Pretty.S with type t := t
  include Comparator.S with type t := t

  include
    Comparable.S
    with type t := t
     and type comparator_witness := comparator_witness
end

module Make (X : Fixed.S) (Meta : Meta) :
  S with type t = Meta.t X.t and module Meta := Meta = struct
  module Basic = struct
    type t = Meta.t X.t

    let pp ppf x = X.pp Meta.pp ppf x
    let compare x y = X.compare Meta.compare x y
    let sexp_of_t x = X.sexp_of_t Meta.sexp_of_t x
    let t_of_sexp x = X.t_of_sexp Meta.t_of_sexp x

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end

module Make2 (X : Fixed.S2) (First : S) (Meta : Meta) :
  S with type t = (First.Meta.t, Meta.t) X.t and module Meta := Meta = struct
  module Basic = struct
    type nonrec t = (First.Meta.t, Meta.t) X.t

    let pp ppf x = X.pp First.Meta.pp Meta.pp ppf x
    let compare x y = X.compare First.Meta.compare Meta.compare x y
    let sexp_of_t x = X.sexp_of_t First.Meta.sexp_of_t Meta.sexp_of_t x
    let t_of_sexp x = X.t_of_sexp First.Meta.t_of_sexp Meta.t_of_sexp x

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end
