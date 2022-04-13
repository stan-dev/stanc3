(** Setup of our compiler errors *)

open Core_kernel
module Str = Re.Str

(** Our type of syntax error information *)
type syntax_error =
  | Lexing of Middle.Location.t
  | UnexpectedEOF of Middle.Location.t
  | Include of string * Middle.Location.t
  | Parsing of string * Middle.Location_span.t

(** Exception for Syntax Errors *)
exception SyntaxError of syntax_error

(** Exception [SemanticError (msg, loc)] indicates a semantic error with message
    [msg], occurring in location [loc]. *)
exception SemanticError of Semantic_error.t

type t =
  | FileNotFound of string
  | Syntax_error of syntax_error
  | Semantic_error of Semantic_error.t

let pp_context_with_message ppf (msg, loc) =
  Fmt.pf ppf "%a@,%s" (Fmt.option Fmt.string)
    (Middle.Location.context_to_string loc)
    msg

let pp_semantic_error ?printed_filename ppf err =
  let loc_span = Semantic_error.location err in
  Fmt.pf ppf "Semantic error in %s:@;%a"
    (Middle.Location_span.to_string ?printed_filename loc_span)
    pp_context_with_message
    (Fmt.str "%a@." Semantic_error.pp err, loc_span.begin_loc)

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ?printed_filename ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "Syntax error in %s, parsing error:@,%a"
        (Middle.Location_span.to_string ?printed_filename loc_span)
        pp_context_with_message
        (message, loc_span.begin_loc)
  | Lexing loc ->
      Fmt.pf ppf "Syntax error in %s, lexing error:@,%a@."
        (Middle.Location.to_string ?printed_filename
           {loc with col_num= loc.col_num - 1} )
        pp_context_with_message
        ("Invalid character found.", loc)
  | UnexpectedEOF loc ->
      Fmt.pf ppf "Syntax error in %s, lexing error:@,%a@."
        (Middle.Location.to_string ?printed_filename
           {loc with col_num= loc.col_num - 1} )
        pp_context_with_message
        ("Unexpected end of input", loc)
  | Include (message, loc) ->
      Fmt.pf ppf "Syntax error in %s, include error:@,%a@."
        (Middle.Location.to_string loc ?printed_filename)
        pp_context_with_message (message, loc)

let pp ?printed_filename ppf = function
  | FileNotFound f ->
      Fmt.pf ppf "Error: file '%s' not found or cannot be opened@." f
  | Syntax_error e -> pp_syntax_error ?printed_filename ppf e
  | Semantic_error e -> pp_semantic_error ?printed_filename ppf e

let to_string = Fmt.str "%a" (pp ?printed_filename:None)
