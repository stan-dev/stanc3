(** Delimited locations in source code *)

open Core

type t = {begin_loc: Location.t; end_loc: Location.t}
[@@deriving sexp, hash, compare]

let empty = {begin_loc= Location.empty; end_loc= Location.empty}
let merge left right = {begin_loc= left.begin_loc; end_loc= right.end_loc}

(** Render a location_span as a string *)
let to_string ?printed_filename {begin_loc; end_loc} =
  let end_loc_str =
    match begin_loc.included_from with
    | None ->
        " to "
        ^ Location.to_string ?printed_filename
            ~print_file:(not @@ String.equal begin_loc.filename end_loc.filename)
            ~print_line:(begin_loc.line_num <> end_loc.line_num)
            end_loc
    | _ -> "" in
  Location.to_string ?printed_filename begin_loc ^ end_loc_str

module Comparator = Comparator.Make (struct
  type nonrec t = t

  let compare = compare
  let sexp_of_t = sexp_of_t
end)

include Comparator

include Comparable.Make_using_comparator (struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp

  include Comparator
end)
