open! Core

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings
let add_warning span message = warnings := (span, message) :: !warnings

let empty file =
  add_warning Middle.Location_span.empty
    ("Empty file '" ^ file
   ^ "' detected; this is a valid stan model but likely unintended!")
