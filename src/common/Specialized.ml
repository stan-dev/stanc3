open Core_kernel

module type S = sig
  type t [@@deriving compare, sexp]

  module Meta : Meta.S
  include Pretty.S with type t := t
  include Comparator.S with type t := t

  include
    Comparable.S
    with type t := t
     and type comparator_witness := comparator_witness
end

module Make (Fixed : Fix.S) (Meta : Meta.S) :
  S with type t = Meta.t Fixed.t and module Meta := Meta = struct
  module Basic = struct
    type t = Meta.t Fixed.t

    let pp ppf x = Fixed.pp Meta.pp ppf x
    let compare x y = Fixed.compare Meta.compare x y
    let sexp_of_t x = Fixed.sexp_of_t Meta.sexp_of_t x
    let t_of_sexp x = Fixed.t_of_sexp Meta.t_of_sexp x

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end

module Make2 (Fixed : Fix.S2) (First : S) (Meta : Meta.S) :
  S with type t = (First.Meta.t, Meta.t) Fixed.t and module Meta := Meta =
struct
  module Basic = struct
    type t = (First.Meta.t, Meta.t) Fixed.t

    let pp ppf x = Fixed.pp First.Meta.pp Meta.pp ppf x
    let compare x y = Fixed.compare First.Meta.compare Meta.compare x y
    let sexp_of_t x = Fixed.sexp_of_t First.Meta.sexp_of_t Meta.sexp_of_t x
    let t_of_sexp x = Fixed.t_of_sexp First.Meta.t_of_sexp Meta.t_of_sexp x

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end
