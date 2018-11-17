(** Some plumbing for our compiler errors *)

(** Source code locations. *)
type location =
  | Location of Lexing.position * Lexing.position  (** delimited location *)
  | Nowhere  (** no location *)

(** [make_location p1 p2] creates a location which starts at [p1] and ends at [p2]. *)
let make_location loc1 loc2 = Location (loc1, loc2)

(** Convert a [Lexing.lexbuf] location to a [location] *)
let location_of_lex lex =
  Location (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

(** Exception [Error (loc, err, msg)] indicates an error of type [err] with error message
    [msg], occurring at location [loc]. *)
exception Error of (location * string * string)

(** [error ~loc ~kind] raises an error of the given [kind] which is caught by the toplevel and
prints the given message. The [kfprintf] magic allows one to write [msg] using a format string. *)
let error ?(kind = "Error") ?(loc = Nowhere) =
  let k _ =
    let msg = Format.flush_str_formatter () in
    raise (Error (loc, kind, msg))
  in
  Format.kfprintf k Format.str_formatter

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
        Format.fprintf ppf "file %S, line %d, characters %d-%d" filename
          begin_line begin_char end_char
      else
        Format.fprintf ppf "line %d, characters %d-%d" (begin_line - 1)
          begin_char end_char

(** A fatal error reported by the toplevel. *)
let fatal_error msg = error ~kind:"Fatal error" msg

(** A syntax error reported by the toplevel *)
let syntax_error ?loc msg = error ~kind:"Syntax error" ?loc msg

(* A semantic error reported by the toplevel *)
let semantic_error ?loc msg =
  error ~kind:"Semantic error" ?loc (Scanf.format_from_string msg "")

(** Print a message at a given location [loc] of message type [msg_type]. *)
let print_message ?(loc = Nowhere) msg_type =
  match loc with
  | Location _ ->
      Format.eprintf "%s at %t:@\n" msg_type (print_location loc) ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter
  | Nowhere ->
      Format.eprintf "%s: " msg_type ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter

(** Print the caught error *)
let print_error (loc, err_type, msg) = print_message ~loc err_type "%s" msg

(** Parser wrapper that catches syntax-related errors and converts them to errors. *)
let wrap_syntax_errors parser lex =
  try parser lex with
  | Failure _ -> syntax_error ~loc:(location_of_lex lex) "unrecognised symbol"
  | _ -> syntax_error ~loc:(location_of_lex lex) "syntax error"

type parse_error =
  | Lexing of string * Lexing.position
  | Parsing of string option * Lexing.position * Lexing.position

exception SyntaxError of parse_error

let position {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  let file = pos_fname in
  let line = pos_lnum in
  let character = pos_cnum - pos_bol in
  (file, line, character)

let nth_line file line =
  try
    let input = open_in file in
    for i = 1 to line - 1 do
      ignore (input_line input)
    done ;
    let result = input_line input in
    close_in input ; Some result
  with _ -> None

let report_error lexbuf = function
  | Parsing (message, start_pos, end_pos) -> (
      let file, start_line, start_character = position start_pos in
      let _, curr_line, curr_character = position end_pos in
      let open Printf in
      let lines =
        if curr_line = start_line then sprintf "line %d" curr_line
        else sprintf "lines %d-%d" start_line curr_line
      in
      let characters =
        if curr_line = start_line then
          sprintf "characters %d-%d" start_character curr_character
        else sprintf "character %d" start_character
      in
      Printf.eprintf "Syntax error at file %S, %s, %s, parsing error:\n%!" file
        lines characters ;
      ( match nth_line file curr_line with
      | None -> ()
      | Some line -> Printf.eprintf "> %s\n" line ) ;
      match message with
      | None -> ()
      | Some error_message -> prerr_endline error_message )
  | Lexing (invalid_input, err_pos) ->
      let file, line, character = position err_pos in
      Printf.eprintf
        "Syntax error at file %S, line %d, character %d, lexing error:\n" file
        line character ;
      ( match nth_line file line with
      | None -> ()
      | Some line -> Printf.eprintf "> %s\n" line ) ;
      Printf.eprintf "Invalid input %S\n%!" invalid_input
