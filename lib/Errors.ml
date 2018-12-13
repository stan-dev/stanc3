(** Setup of our compiler errors *)
open Ast

(** Our type of syntax error information *)
type parse_error =
  | Lexing of string * Lexing.position
  | Includes of string * Lexing.position
  | Parsing of string option * Lexing.position * Lexing.position

(** Exception for Syntax Errors *)
exception SyntaxError of parse_error

(** Exception [SemanticError (loc, msg)] indicates a semantic error with message
    [msg], occurring at location [loc]. *)
exception SemanticError of (location * string)

(** Exception [FatalError [msg]] indicates an error that should never happen with message
    [msg]. *)
exception FatalError of string

let position {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  let file = pos_fname in
  let line = pos_lnum in
  let column = pos_cnum - pos_bol in
  (file, line, column)

let error_context file line column =
  try
    let bare_file =
      List.hd (Str.split (Str.regexp "\" included from \"") file)
    in
    let input = open_in bare_file in
    for _ = 1 to line - 3 do
      ignore (input_line input)
    done ;
    let open Printf in
    let line_2_before =
      if line > 2 then sprintf "%6d:  %s\n" (line - 2) (input_line input)
      else ""
    in
    let line_before =
      if line > 1 then sprintf "%6d:  %s\n" (line - 1) (input_line input)
      else ""
    in
    let our_line = sprintf "%6d:  %s\n" line (input_line input) in
    let cursor_line = String.make (column + 9) ' ' ^ "^\n" in
    let line_after =
      try sprintf "%6d:  %s\n" (line + 1) (input_line input) with _ -> ""
    in
    let line_2_after =
      try sprintf "%6d:  %s\n" (line + 2) (input_line input) with _ -> ""
    in
    close_in input ;
    Some
      (sprintf
         "   -------------------------------------------------\n\
          %s%s%s%s%s%s   -------------------------------------------------\n"
         line_2_before line_before our_line cursor_line line_after line_2_after)
  with _ -> None

(** A syntax error message used when handling a SyntaxError *)
let report_syntax_error = function
  | Parsing (message, start_pos, end_pos) -> (
      let file, start_line, start_column = position start_pos in
      let _, curr_line, curr_column = position end_pos in
      let open Printf in
      let lines =
        if curr_line = start_line then sprintf "line %d" curr_line
        else sprintf "lines %d-%d" start_line curr_line
      in
      let columns =
        if curr_line = start_line then
          sprintf "columns %d-%d" start_column curr_column
        else sprintf "column %d" start_column
      in
      Printf.eprintf
        "\nSyntax error at file \"%s\", %s, %s, parsing error:\n%!" file lines
        columns ;
      ( match error_context file curr_line curr_column with
      | None -> ()
      | Some line -> Printf.eprintf "%s\n" line ) ;
      match message with
      | None -> Printf.eprintf "\n"
      | Some error_message -> prerr_endline error_message )
  | Lexing (invalid_input, err_pos) ->
      let file, line, column = position err_pos in
      Printf.eprintf
        "\nSyntax error at file \"%s\", line %d, column %d, lexing error:\n"
        file line (column - 1) ;
      ( match error_context file line (column - 1) with
      | None -> ()
      | Some line -> Printf.eprintf "%s\n" line ) ;
      Printf.eprintf "Invalid input %S\n%!\n" invalid_input
  | Includes (msg, err_pos) ->
      let file, line, column = position err_pos in
      Printf.eprintf
        "\nSyntax error at file \"%s\", line %d, column %d, includes error:\n"
        file line column ;
      ( match error_context file line column with
      | None -> ()
      | Some line -> Printf.eprintf "%s\n" line ) ;
      Printf.eprintf "%s\n" msg

(** Print a location *)
let print_location loc ppf =
  match loc with
  | Nowhere -> Format.fprintf ppf "unknown location"
  | Location (begin_pos, end_pos) ->
      let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let begin_line = begin_pos.Lexing.pos_lnum in
      let filename = begin_pos.Lexing.pos_fname in
      if String.length filename != 0 then
        Format.fprintf ppf "file \"%s\", line %d, columns %d-%d" filename
          begin_line begin_char end_char
      else
        Format.fprintf ppf "line %d, columns %d-%d" (begin_line - 1) begin_char
          end_char

(** A semantic error message used when handling a SemanticError *)
let report_semantic_error (loc, msg) =
  match loc with
  | Location ({pos_fname= file; pos_lnum= line; pos_cnum= pos; pos_bol= bol}, _)
    ->
      Format.eprintf "\n%s at %t:@\n" "Semantic error" (print_location loc) ;
      ( match error_context file line (pos - bol) with
      | None -> ()
      | Some line -> Format.eprintf "%s\n" line ) ;
      Format.kfprintf
        (fun ppf -> Format.fprintf ppf "@.")
        Format.err_formatter "%s\n" msg
  | Nowhere ->
      Format.eprintf "\n%s: " "Semantic error" ;
      Format.kfprintf
        (fun ppf -> Format.fprintf ppf "@.")
        Format.err_formatter "%s\n" msg

(* A semantic error reported by the toplevel *)
let semantic_error ?(loc = Nowhere) msg = raise (SemanticError (loc, msg))

(* A fatal error reported by the toplevel *)
let fatal_error ?(msg = "") _ =
  raise (FatalError ("This should never happen. Please file a bug. " ^ msg))
