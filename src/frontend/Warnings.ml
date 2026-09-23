module Location_span = Middle.Location_span
module Location = Middle.Location

type t = Location_span.t * string

let to_grace ?printed_filename ?code (span, message) =
  let open Grace.Diagnostic in
  let range, included =
    Diagnostic.range_of_loc_span ?printed_filename ?code span in
  let message = Fmt.str "@[%a@]" Fmt.text message in
  let message ppf = Fmt.lines ppf message in
  let labels = Label.primary ~range message :: included in
  create Warning ~labels message

let pp ?printed_filename ?code ppf (span, message) =
  let diagnostic = to_grace ?printed_filename ?code (span, message) in
  Fmt.pf ppf "%a@." Diagnostic.pp_compact diagnostic

let pp_warnings ?printed_filename ?code ppf warnings =
  if not (List.is_empty warnings) then
    Fmt.(
      pf ppf "@[<v>%a@]" (list ~sep:cut (pp ?printed_filename ?code)) warnings)
