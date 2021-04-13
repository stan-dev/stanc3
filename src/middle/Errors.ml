(** Setup of our compiler errors *)

open Core_kernel
module Str = Re.Str

(** Our type of syntax error information *)
type syntax_error =
  | Lexing of string * Location.t
  | Include of string * Location.t
  | Parsing of string * Location_span.t

(** Exception for Syntax Errors *)
exception SyntaxError of syntax_error

(** Exception [SemanticError (msg, loc)] indicates a semantic error with message
    [msg], occurring in location [loc]. *)
exception SemanticError of Semantic_error.t

(** Exception [FatalError [msg]] indicates an error that should never happen with message
    [msg]. *)
exception FatalError of string

type t =
  | FileNotFound of string
  | Syntax_error of syntax_error
  | Semantic_error of Semantic_error.t

(* A fatal error reported by the toplevel *)
let fatal_error ?(msg = "") _ =
  raise (FatalError ("This should never happen. Please file a bug. " ^ msg))

let pp_context_with_message ppf (msg, loc) =
  Fmt.pf ppf "%a@,%s" (Fmt.option Fmt.string)
    (Location.context_to_string loc)
    msg

let pp_semantic_error ?printed_filename ppf err =
  let loc_span = Semantic_error.location err in
  Fmt.pf ppf "Semantic error in %s:@;%a"
    (Location_span.to_string ?printed_filename loc_span)
    pp_context_with_message
    (Fmt.strf "%a" Semantic_error.pp err, loc_span.begin_loc)

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ?printed_filename ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "Syntax error in %s, parsing error:@,%a"
        (Location_span.to_string ?printed_filename loc_span)
        pp_context_with_message
        (message, loc_span.begin_loc)
  | Lexing (_, loc) ->
      Fmt.pf ppf "Syntax error in %s, lexing error:@,%a"
        (Location.to_string ?printed_filename
           {loc with col_num= loc.col_num - 1})
        pp_context_with_message
        ("Invalid character found.", loc)
  | Include (message, loc) ->
      Fmt.pf ppf "Syntax error in %s, include error:@,%a"
        (Location.to_string loc ?printed_filename)
        pp_context_with_message (message, loc)

let pp ?printed_filename ppf = function
  | FileNotFound f -> Fmt.pf ppf "Cannot not open file %s@." f
  | Syntax_error e -> pp_syntax_error ?printed_filename ppf e
  | Semantic_error e -> pp_semantic_error ?printed_filename ppf e

let to_string = Fmt.strf "%a" (pp ?printed_filename:None)
