type t = Location_span.t * string

let pp ?printed_filename ppf (span, message) =
  let loc_str =
    if span = Location_span.empty then ""
    else " in " ^ Location.to_string ?printed_filename span.begin_loc
  in
  Fmt.pf ppf "@[<hov>Warning%s: %s@]" loc_str message

let pp_warnings ?printed_filename ppf warnings =
  Fmt.(
    pf ppf "@[<v>%a@]%a" (list ~sep:cut (pp ?printed_filename)) warnings cut ())
