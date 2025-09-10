(** Our type of syntax error information *)
type t =
  | Lexing of Middle.Location_span.t
  | UnexpectedEOF of Middle.Location_span.t
  | Include of string * Middle.Location_span.t
  | Parsing of string * Middle.Location_span.t

let location = function
  | Parsing (_, loc_span)
   |Lexing loc_span
   |UnexpectedEOF loc_span
   |Include (_, loc_span) ->
      loc_span

let kind = function
  | Parsing _ -> "parsing error"
  | UnexpectedEOF _ | Lexing _ -> "lexing error"
  | Include _ -> "include error"

(** A syntax error message used when handling a SyntaxError *)
let pp ppf = function
  | Parsing (message, _) -> Fmt.string ppf message
  | Lexing _ -> Fmt.pf ppf "Invalid character found.@."
  | UnexpectedEOF _ -> Fmt.pf ppf "Unexpected end of input.@."
  | Include (message, _) -> Fmt.pf ppf "%s@." message

exception ParserException of string * Middle.Location_span.t
exception UnexpectedEOF of Middle.Location_span.t
exception UnexpectedCharacter of Middle.Location_span.t
exception IncludeError of string * Middle.Location_span.t

let unexpected_eof loc = raise (UnexpectedEOF loc)
let unexpected_character loc = raise (UnexpectedCharacter loc)
let include_error msg loc = raise (IncludeError (msg, loc))
let parse_error msg loc = raise (ParserException (msg, loc))

let try_with f =
  try Ok (f ()) with
  | ParserException (msg, loc) -> Error (Parsing (msg, loc))
  | UnexpectedEOF loc -> Error (UnexpectedEOF loc)
  | UnexpectedCharacter loc -> Error (Lexing loc)
  | IncludeError (msg, loc) -> Error (Include (msg, loc))
