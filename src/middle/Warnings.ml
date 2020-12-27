open! Core_kernel

type t = Location_span.t * string

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings

let deprecated token (pos, message) =
  (* TODO(seantalts): should we only print deprecation warnings once per token? *)
  let begin_pos =
    {pos with Lexing.pos_cnum= pos.Lexing.pos_cnum - String.length token}
  in
  let end_pos = {begin_pos with Lexing.pos_cnum= pos.pos_cnum - 1} in
  let span =
    Location_span.of_positions_opt begin_pos end_pos
    |> Option.value ~default:Location_span.empty
  in
  warnings := (span, message) :: !warnings

let pp ?printed_filename ppf (span, message) =
  Fmt.pf ppf "@[<hov 2>Warning in %s: %s@]"
    (Location.to_string ?printed_filename span.Location_span.begin_loc)
    message

let pp_warnings ?printed_filename = Fmt.(list ~sep:cut (pp ?printed_filename))
