open! Core

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings
let add_warning span message = warnings := (span, message) :: !warnings

let empty () =
  let Middle.Location_span.{begin_loc; end_loc} =
    Preprocessor.current_location () in
  let begin_loc = {begin_loc with line_num= 1; col_num= 0} in
  add_warning
    Middle.Location_span.{begin_loc; end_loc}
    "Empty model detected; this is a valid Stan model but likely unintended!"
