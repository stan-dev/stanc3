module Location_span = Middle.Location_span
module Location = Middle.Location

type t = Location_span.t * string

let purple = Fmt.styled (`Fg `Magenta) Fmt.string

let pp ?printed_filename ppf (span, message) =
  Fmt.pf ppf "@[<v4>%a in @[%a@]:@ @[%a@]@]" purple "Warning"
    (Location_span.pp ?printed_filename)
    span Fmt.text message

let pp_warnings ?printed_filename ppf warnings =
  if not (Core.List.is_empty warnings) then
    Fmt.(pf ppf "@[<v>%a@]@\n" (list ~sep:cut (pp ?printed_filename)) warnings)
