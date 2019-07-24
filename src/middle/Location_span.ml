open Core_kernel

(** Source code locations *)
type location =
  { filename: string
  ; line_num: int
  ; col_num: int
  ; included_from: location option }
[@@deriving sexp, hash, compare]

(** Delimited locations *)
type t = {begin_loc: location; end_loc: location}
[@@deriving sexp, hash, compare]

let no_loc = {filename= ""; line_num= 0; col_num= 0; included_from= None}
let no_span = {begin_loc= no_loc; end_loc= no_loc}

let rec string_of_location ?(print_file = true) ?(print_line = true) loc =
  let open Format in
  let file = if print_file then sprintf "'%s', " loc.filename else "" in
  let line = if print_line then sprintf "line %d, " loc.line_num else "" in
  let incl =
    match loc.included_from with
    | Some loc2 -> sprintf ", included from\n%s" (string_of_location loc2)
    | None -> ""
  in
  sprintf "%s%scolumn %d%s" file line loc.col_num incl

(** Render a location_span as a string *)
let string_of_t {begin_loc; end_loc} =
  let end_loc_str =
    match begin_loc.included_from with
    | None ->
        " to "
        ^ string_of_location
            ~print_file:(begin_loc.filename <> end_loc.filename)
            ~print_line:(begin_loc.line_num <> end_loc.line_num)
            end_loc
    | Some _ -> ""
  in
  string_of_location begin_loc ^ end_loc_str

let merge left right = {begin_loc= left.begin_loc; end_loc= right.end_loc}
