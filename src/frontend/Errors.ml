(** Setup of our compiler errors *)

open Core_kernel
open Middle
module Str = Re.Str

(** Our type of syntax error information *)
type parse_error =
  | Lexing of string * Location.t
  | Include of string * Location.t
  | Parsing of string * Location_span.t

(** Exception for Syntax Errors *)
exception SyntaxError of parse_error

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
let print_context_and_message message loc =
  ( match Location.context_to_string loc with
  | None -> ()
  | Some line -> Printf.eprintf "%s\n" line ) ;
  Printf.eprintf "%s\n\n" message

(** A syntax error message used when handling a SyntaxError *)
let report_syntax_error = function
  | Parsing (message, loc_span) ->
      Printf.eprintf "\nSyntax error in %s, parsing error:\n"
        (Location_span.to_string loc_span) ;
      print_context_and_message message loc_span.end_loc
  | Lexing (_, loc) ->
      Printf.eprintf "\nSyntax error in %s, lexing error:\n"
        (Location.to_string {loc with col_num= loc.col_num - 1}) ;
      print_context_and_message "Invalid character found." loc
  | Include (message, loc) ->
      Printf.eprintf "\nSyntax error in %s, include error:\n"
        (Location.to_string loc) ;
      print_context_and_message message loc

let report_parsing_error (message, loc_span) =
  Printf.eprintf "\nSyntax error in %s, parsing error:\n"
    (Location_span.to_string loc_span) ;
  print_context_and_message message loc_span.end_loc

(** A semantic error message used when handling a SemanticError *)
let report_semantic_error (message, loc_span) =
  Printf.eprintf "\n%s in %s:\n" "Semantic error"
    (Location_span.to_string loc_span) ;
  print_context_and_message message loc_span.begin_loc

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
  if !print_warnings then (
    Printf.eprintf "\nWarning: deprecated language construct used in %s:\n"
    @@ Location.to_string loc ;
    print_context_and_message message loc )
