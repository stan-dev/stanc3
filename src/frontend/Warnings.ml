module Location_span = Middle.Location_span
module Location = Middle.Location

type t = Location_span.t * string

let purple = Fmt.styled (`Fg `Magenta) Fmt.string

let pp ?printed_filename ppf (span, message) =
  let maybe_loc ppf span =
    if span = Location_span.empty then Fmt.pf ppf ": @[<-5>"
    else Fmt.pf ppf " in @[%a@]:@ @[" (Location_span.pp ?printed_filename) span
  in
  Fmt.pf ppf "@[<v4>%a%a%a@]@]" purple "Warning" maybe_loc span Fmt.text message

let pp_warnings ?printed_filename ppf warnings =
  if not (List.is_empty warnings) then
    Fmt.(pf ppf "@[<v>%a@]@\n" (list ~sep:cut (pp ?printed_filename)) warnings)
