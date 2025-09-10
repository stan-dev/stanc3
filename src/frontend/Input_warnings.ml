open! Core

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings
let add_warning span message = warnings := (span, message) :: !warnings

let empty () =
  add_warning
    (Preprocessor.current_location ())
    "Empty model detected; this is a valid Stan model but likely unintended!"
