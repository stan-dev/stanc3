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
let pp_context_and_message ppf (message, loc) =
  Fmt.pf ppf "@[<v>%a@,%s@,@]" (Fmt.option Fmt.string)
    (Location.context_to_string loc)
    message

let pp_semantic_error ppf (message, loc_span) =
  Fmt.pf ppf "@[<v>@;Semantic error in %s:@;%a@]@."
    (Location_span.to_string loc_span)
    pp_context_and_message
    (message, loc_span.begin_loc)

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, parsing error:@,%a@]@."
        (Location_span.to_string loc_span)
        pp_context_and_message
        (message, loc_span.end_loc)
  | Lexing (_, loc) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, lexing error:@,%a@]@."
        (Location.to_string {loc with col_num= loc.col_num - 1})
        pp_context_and_message
        ("Invalid character found.", loc)
  | Include (message, loc) ->
      Fmt.pf ppf "@[<v>@,Syntax error in %s, include error:@,%a@]@."
        (Location.to_string loc) pp_context_and_message (message, loc)

(** Switch to control whether warning messages should be printed to stderr (or discarded in case set to false) *)
let print_warnings = ref true

let without_warnings function_name args =
  print_warnings := false ;
  let out = function_name args in
  print_warnings := true ;
  out

(* Warn that a language feature is deprecated *)
let warn_deprecated (pos, message) =
  let loc =
    Location.of_position_opt {pos with Lexing.pos_cnum= pos.Lexing.pos_cnum - 1}
    |> Option.value ~default:Location.empty
  in
  if !print_warnings then
    Fmt.pf Fmt.stderr
      "@[<v>@,Warning: deprecated language construct used in %s:@,%a@]@."
      (Location.to_string loc) pp_context_and_message (message, loc)
