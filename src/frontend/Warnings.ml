module Location_span = Middle.Location_span
module Location = Middle.Location

type t = Location_span.t * string

let purple = Fmt.styled (`Fg `Magenta) Fmt.string

let pp ?printed_filename ppf (span, message) =
  let maybe_loc =
    Fmt.if' (span <> Location_span.empty) (fun ppf loc ->
        Fmt.pf ppf " in %a" (Location.pp printed_filename) loc) in
  Fmt.pf ppf "@[<hov 4>%a%a: %a@]" purple "Warning" maybe_loc span.begin_loc
    Fmt.text message

let pp_warnings ?printed_filename ppf warnings =
  if List.length warnings > 0 then
    Fmt.(pf ppf "@[<v>%a@]@\n" (list ~sep:cut (pp ?printed_filename)) warnings)
