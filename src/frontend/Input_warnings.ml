open! Core_kernel

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings
let add_warning span message = warnings := (span, message) :: !warnings

let empty file =
  add_warning Middle.Location_span.empty
    ( "Empty file '" ^ file
    ^ "' detected; this is a valid stan model but likely unintended!" )

let deprecated token (pos, message) =
  (* TODO(seantalts): should we only print deprecation warnings once per token? *)
  let begin_pos =
    {pos with Lexing.pos_cnum= pos.Lexing.pos_cnum - String.length token}
  in
  let end_pos = {begin_pos with Lexing.pos_cnum= pos.pos_cnum - 1} in
  let span =
    Middle.Location_span.of_positions_opt begin_pos end_pos
    |> Option.value ~default:Middle.Location_span.empty
  in
  add_warning span message
