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

module Make (X : Fix.S) (Meta : Meta.S) :
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

module Make2 (X : Fix.S2) (First : S) (Meta : Meta.S) :
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
