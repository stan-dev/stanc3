open Core_kernel

(** Comparable, pretty-printable labels used to annotate intermediate
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
  val prev : t -> t
end

(** Integer labels *)
module Int_label = struct
  module Basic = struct
    type t = int [@@deriving compare, hash, sexp]

    let init = 0
    let next label = label + 1
    let prev label = label - 1
    let pp ppf x = Fmt.int ppf x

    include Comparator.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)
  end

  include Basic
  include Comparable.Make_using_comparator (Basic)
end
