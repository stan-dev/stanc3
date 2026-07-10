(** [Base]'s [Set.Poly] and [Map.Poly] do not provide the sexp converters that
    Core's versions do. Opening this module (after [Base]) shadows [Set] and
    [Map] with versions whose [Poly] submodules support [sexp_of_t], printing
    in the same format as Core: a set as the list of its elements, a map as a
    list of [(key value)] pairs. *)

open Base

module Set = struct
  include Set

  module Poly = struct
    include Set.Poly

    let sexp_of_t sexp_of_elt s =
      List.sexp_of_t sexp_of_elt (Base.Set.to_list s)
  end
end

module Map = struct
  include Map

  module Poly = struct
    include Map.Poly

    let sexp_of_t sexp_of_key sexp_of_data m =
      List.sexp_of_t
        (fun (k, v) -> Sexp.List [sexp_of_key k; sexp_of_data v])
        (Base.Map.to_alist m)
  end
end
