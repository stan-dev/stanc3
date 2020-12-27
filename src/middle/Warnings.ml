open! Core_kernel

type t = Location.t * string

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings

let deprecated (pos, message) =
  let loc =
    Location.of_position_opt {pos with Lexing.pos_cnum= pos.Lexing.pos_cnum - 1}
    |> Option.value ~default:Location.empty
  in
  warnings := (loc, message) :: !warnings

let pp ?printed_filename ppf (loc, message) =
  Fmt.pf ppf
    "@[<v>@,Warning: deprecated language construct used in %s:@,%a@]@."
    (Location.to_string ?printed_filename loc)
    Errors.pp_context_and_message (message, loc)

let pp_warnings ?printed_filename = Fmt.(list ~sep:nop (pp ?printed_filename))
