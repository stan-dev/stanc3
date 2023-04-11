open Core_kernel

type t = {begin_loc: Location.t; end_loc: Location.t}
[@@deriving sexp, hash, compare]

let empty = {begin_loc= Location.empty; end_loc= Location.empty}
let merge left right = {begin_loc= left.begin_loc; end_loc= right.end_loc}

let to_string ?printed_filename {begin_loc; end_loc} =
  let end_loc_str =
    match begin_loc.included_from with
    | None ->
        " to "
        ^ Location.to_string
            ~print_file:(not @@ String.equal begin_loc.filename end_loc.filename)
            ~print_line:(begin_loc.line_num <> end_loc.line_num)
            end_loc
    | Some _ -> "" in
  Location.to_string ?printed_filename begin_loc ^ end_loc_str

let of_positions_opt start_pos end_pos =
  Option.(
    Location.of_position_opt start_pos
    >>= fun begin_loc ->
    Location.of_position_opt end_pos
    |> map ~f:(fun end_loc -> {begin_loc; end_loc}))

let of_positions_exn (start_pos, end_pos) =
  Option.value_exn (of_positions_opt start_pos end_pos)
