(** Our type of syntax error information *)
type t =
  | Lexing of Middle.Location.t
  | UnexpectedEOF of Middle.Location.t
  | Include of string * Middle.Location.t
  | Parsing of string * Middle.Location_span.t

let location = function
  | Parsing (_, loc_span) -> loc_span
  | Lexing loc | UnexpectedEOF loc | Include (_, loc) ->
      {begin_loc= loc; end_loc= loc}

let kind = function
  | Parsing _ -> "parsing error"
  | UnexpectedEOF _ | Lexing _ -> "lexing error"
  | Include _ -> "include error"

(** A syntax error message used when handling a SyntaxError *)
let pp ppf = function
  | Parsing (message, _) -> Fmt.string ppf message
  | Lexing _ -> Fmt.pf ppf "Invalid character found.@."
  | UnexpectedEOF _ -> Fmt.pf ppf "Unexpected end of input.@."
  | Include (message, _) -> Fmt.string ppf message

exception ParserException of string * Middle.Location_span.t
exception UnexpectedEOF of Middle.Location.t
exception UnexpectedCharacter of Middle.Location.t
exception IncludeError of string * Middle.Location.t

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
