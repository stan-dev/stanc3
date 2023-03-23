(** Setup of our compiler errors *)

open Core_kernel
open Middle
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
exception SemanticError of (string * Location_span.t)

(** Exception [FatalError [msg]] indicates an error that should never happen with message
    [msg]. *)
exception FatalError of string

(* A fatal error reported by the toplevel *)
let fatal_error ?(msg = "") _ =
  raise (FatalError ("This should never happen. Please file a bug. " ^ msg))

(** Return two lines before and after the specified location
    and print a message *)
let pp_context_and_message ?code ppf (message, loc) =
  let open Middle.Location in
  let context_callback =
    match code with
    | Some s -> fun () -> String.split_lines s
    | None -> fun () -> In_channel.read_lines loc.filename
  in
  Fmt.pf ppf "%a@,%s" (Fmt.option Fmt.string)
    (context_to_string context_callback loc)
    message

let pp_semantic_error ?printed_filename ?code ppf (message, loc_span) =
  Fmt.pf ppf "@[<v>@;Semantic error in %s:@;%a@]@."
    (Location_span.to_string ?printed_filename loc_span)
    (pp_context_and_message ?code)
    (message, loc_span.begin_loc)

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ?printed_filename ?code ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, parsing error:@,%a@]@."
        (Location_span.to_string ?printed_filename loc_span)
        (pp_context_and_message ?code)
        (message, loc_span.begin_loc)
  | Lexing (_, loc) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, lexing error:@,%a@]@."
        (Location.to_string ?printed_filename
           {loc with col_num= loc.col_num - 1})
        (pp_context_and_message ?code)
        ("Invalid character found.", loc)
  | Include (message, loc) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, include error:@,%a@]@."
        (Location.to_string ?printed_filename loc)
        (pp_context_and_message ?code)
        (message, loc)
