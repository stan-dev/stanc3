open! Core_kernel
open Middle

type t = Lexing.position * string

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings
let deprecated t = warnings := t :: !warnings

let pp ppf (pos, message) =
  let loc =
    Location.of_position_opt {pos with Lexing.pos_cnum= pos.Lexing.pos_cnum - 1}
    |> Option.value ~default:Location.empty
  in
  Fmt.pf ppf
    "@[<v>@,Warning: deprecated language construct used in %s:@,%a@]@."
    (Location.to_string loc) Errors.pp_context_and_message (message, loc)
