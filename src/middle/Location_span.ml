(** Delimited locations in source code *)

open Core

type t = {begin_loc: Location.t; end_loc: Location.t}
[@@deriving sexp, hash, compare]

let empty = {begin_loc= Location.empty; end_loc= Location.empty}
let merge left right = {begin_loc= left.begin_loc; end_loc= right.end_loc}

(** Render a location_span as a string *)
let pp ?printed_filename ppf {begin_loc; end_loc} =
  let end_loc_pp =
    Fmt.if' (Option.is_none begin_loc.included_from)
      (fun ppf (end_loc : Location.t) ->
        Fmt.pf ppf " to %a"
          (Location.pp ?printed_filename
             ~print_file:
               (not @@ String.equal begin_loc.filename end_loc.filename)
             ~print_line:(begin_loc.line_num <> end_loc.line_num)
             ())
          end_loc) in
  Fmt.pf ppf "%a%a"
    (Location.pp ?printed_filename ())
    begin_loc end_loc_pp end_loc

let to_string ?printed_filename loc_span =
  Fmt.str "%a" (pp ?printed_filename) loc_span

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
