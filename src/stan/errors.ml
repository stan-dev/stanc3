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
