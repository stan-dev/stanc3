(** Some plumbing for our compiler errors *)

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

let report_syntax_error lexbuf = function
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

(** Exception [SemanticError (loc, msg)] indicates a semantic error with message
    [msg], occurring at location [loc]. *)
exception SemanticError of (location * string)

(* TODO: add a kind of fatal error message (which should not take a location) 
   and insert in all places where things could go wrong because of our mistakes.
   Message could suggest to file a bug report. *)

(* A semantic error reported by the toplevel *)
let semantic_error ?(loc = Nowhere) msg = raise (SemanticError (loc, msg))

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

(** Print a semantic error message at a given location [loc]. *)
let print_semantic_error_message ?(loc = Nowhere) =
  match loc with
  | Location _ ->
      Format.eprintf "%s at %t:@\n" "Semantic error" (print_location loc) ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter
  | Nowhere ->
      Format.eprintf "%s: " "Semantic error" ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter

(** Print the caught semantic error *)
let report_semantic_error (loc, msg) =
  print_semantic_error_message ~loc "%s" msg

(* TODO: there's redundancy between the semantic and syntax error string formatting.
   Reuse code there. *)

(* TODO: quote line that triggered the error in case of semantic errors, just like*
   we do for syntax errors. *)
