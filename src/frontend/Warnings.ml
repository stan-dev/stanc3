module Location_span = Middle.Location_span
module Location = Middle.Location

type t = Location_span.t * string

let pp ?printed_filename ppf (span, message) =
  let loc_str =
    if span = Location_span.empty then ""
    else " in " ^ Location.to_string ?printed_filename span.begin_loc in
  Fmt.pf ppf "@[<hov 4>Warning%s: %a@]" loc_str Fmt.text message

let pp_warnings ?printed_filename ppf warnings =
  if List.length warnings > 0 then
    Fmt.(pf ppf "@[<v>%a@]@\n" (list ~sep:cut (pp ?printed_filename)) warnings)
