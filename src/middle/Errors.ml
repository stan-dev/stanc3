(** Setup of our compiler errors *)

open Core_kernel
module Str = Re.Str

(** Our type of syntax error information *)
type syntax_error =
  | Lexing of string * Location.t
  | Include of string * Location.t
  | Parsing of string * Location_span.t
[@@deriving map]

(** Exception for Syntax Errors *)
exception SyntaxError of syntax_error

type semantic_error = Semantic_error.t

(** Exception [SemanticError (msg, loc)] indicates a semantic error with message
    [msg], occurring in location [loc]. *)
exception SemanticError of semantic_error

(** Exception [FatalError [msg]] indicates an error that should never happen with message
    [msg]. *)
exception FatalError of string

type t = Syntax_error of syntax_error | Semantic_error of semantic_error

(* A fatal error reported by the toplevel *)
let fatal_error ?(msg = "") _ =
  raise (FatalError ("This should never happen. Please file a bug. " ^ msg))

let pp_semantic_error ?printed_filename ppf err =
  let loc_span = Semantic_error.location err in
  Fmt.pf ppf "@[<v>@;Semantic error in %s:@;%a@]"
    (Location_span.to_string ?printed_filename loc_span)
    Location.pp_with_message_exn
    (Fmt.strf "%a" Semantic_error.pp err, loc_span.begin_loc)

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ?printed_filename ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, parsing error:@,%a@]"
        (Location_span.to_string ?printed_filename loc_span)
        Location.pp_with_message_exn
        (message, loc_span.begin_loc)
  | Lexing (_, loc) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, lexing error:@,%a@]"
        (Location.to_string ?printed_filename
           {loc with col_num= loc.col_num - 1})
        Location.pp_with_message_exn
        ("Invalid character found.", loc)
  | Include (message, loc) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, include error:@,%a@]"
        (Location.to_string loc ?printed_filename)
        Location.pp_with_message_exn (message, loc)

let pp ?printed_filename ppf = function
  | Syntax_error e -> pp_syntax_error ?printed_filename ppf e
  | Semantic_error e -> pp_semantic_error ?printed_filename ppf e
