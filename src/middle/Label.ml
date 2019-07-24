open Core_kernel

module Basic = struct
  type t = int [@@deriving compare, hash, sexp]

  let pp ppf x = Fmt.int ppf x

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
  end)
end

include Basic
include Comparable.Make_using_comparator (Basic)
