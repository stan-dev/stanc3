(** Setup of our compiler errors *)

open Core_kernel
open Mir

(** Our type of syntax error information *)
type parse_error =
  | Lexing of string * location
  | Include of string * location
  | Parsing of string * location_span

(** Exception for Syntax Errors *)
exception SyntaxError of parse_error

(** Exception [SemanticError (msg, loc)] indicates a semantic error with message
    [msg], occurring in location [loc]. *)
exception SemanticError of (string * location_span)

(** Exception [FatalError [msg]] indicates an error that should never happen with message
    [msg]. *)
exception FatalError of string

(* A semantic error reported by the toplevel *)
let semantic_error ~loc msg = raise (SemanticError (msg, loc))

(* A fatal error reported by the toplevel *)
let fatal_error ?(msg = "") _ =
  raise (FatalError ("This should never happen. Please file a bug. " ^ msg))

(** Parse a string into a location *)
let rec parse_location str =
  let split_str =
    Str.bounded_split
      (Str.regexp "file \\|, line \\|, column \\|, included from\n")
      str 4
  in
  match split_str with
  | [fname; linenum_str; colnum_str] ->
      { filename= fname
      ; line_num= int_of_string linenum_str
      ; col_num= int_of_string colnum_str
      ; included_from= None }
  | [fname; linenum_str; colnum_str; included_from_str] ->
      { filename= fname
      ; line_num= int_of_string linenum_str
      ; col_num= int_of_string colnum_str
      ; included_from= Some (parse_location included_from_str) }
  | _ -> fatal_error ()

(** Take the AST.location corresponding to a Lexing.position *)
let location_of_position = function
  | {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} -> (
      let split_fname =
        Str.bounded_split (Str.regexp ", included from\nfile ") pos_fname 2
      in
      match split_fname with
      | [] -> fatal_error ()
      | fname1 :: fnames ->
          { filename= fname1
          ; line_num= pos_lnum
          ; col_num= pos_cnum - pos_bol
          ; included_from=
              ( match fnames with
              | [] -> None
              | fnames :: _ -> Some (parse_location fnames) ) } )

(** Take the AST.location_span corresponding to a pair of Lexing.position's *)
let loc_span_of_pos start_pos end_pos =
  { begin_loc= location_of_position start_pos
  ; end_loc= location_of_position end_pos }

(** Return two lines before and after the specified location. *)
let print_context {filename; line_num; col_num; _} =
  try
    let open In_channel in
    let input = create filename in
    for _ = 1 to line_num - 3 do
      ignore (input_line_exn input)
    done ;
    let get_line num =
      if num > 0 then
        match input_line input with
        | Some input -> Printf.sprintf "%6d:  %s\n" num input
        | _ -> ""
      else ""
    in
    let line_2_before = get_line (line_num - 2) in
    let line_before = get_line (line_num - 1) in
    let our_line = get_line line_num in
    let cursor_line = String.make (col_num + 9) ' ' ^ "^\n" in
    let line_after = get_line (line_num + 1) in
    let line_2_after = get_line (line_num + 2) in
    close input ;
    Some
      (Printf.sprintf
         "   -------------------------------------------------\n\
          %s%s%s%s%s%s   -------------------------------------------------\n"
         line_2_before line_before our_line cursor_line line_after line_2_after)
  with _ -> None

(** Return two lines before and after the specified location
    and print a message *)
let print_context_and_message message loc =
  ( match print_context loc with
  | None -> ()
  | Some line -> Printf.eprintf "%s\n" line ) ;
  Printf.eprintf "%s\n\n" message

(** A syntax error message used when handling a SyntaxError *)
let report_syntax_error = function
  | Parsing (message, loc_span) ->
      Printf.eprintf "\nSyntax error in %s, parsing error:\n"
        (string_of_location_span loc_span) ;
      print_context_and_message message loc_span.end_loc
  | Lexing (_, loc) ->
      Printf.eprintf "\nSyntax error in %s, lexing error:\n"
        (string_of_location {loc with col_num= loc.col_num - 1}) ;
      print_context_and_message "Invalid character found." loc
  | Include (message, loc) ->
      Printf.eprintf "\nSyntax error in %s, include error:\n"
        (string_of_location loc) ;
      print_context_and_message message loc

(** A semantic error message used when handling a SemanticError *)
let report_semantic_error (message, loc_span) =
  Printf.eprintf "\n%s in %s:\n" "Semantic error"
    (string_of_location_span loc_span) ;
  print_context_and_message message loc_span.begin_loc

(* Warn that a language feature is deprecated *)
let warn_deprecated (pos, message) =
  let loc =
    location_of_position {pos with Lexing.pos_cnum= pos.Lexing.pos_cnum - 1}
  in
  Printf.eprintf "\nWarning: deprecated language construct used in %s:\n"
    (string_of_location loc) ;
  print_context_and_message message loc

(* TESTS *)
let%expect_test "location string equivalence 1" =
  let str =
    "file xxx.stan, line 245, column 13, included from\n\
     file yyy.stan, line 666, column 42, included from\n\
     file zzz.stan, line 24, column 77"
  in
  print_endline (string_of_location (parse_location str)) ;
  [%expect
    {|
      file xxx.stan, line 245, column 13, included from
      file yyy.stan, line 666, column 42, included from
      file zzz.stan, line 24, column 77 |}]

let%expect_test "location string equivalence 2" =
  let loc =
    { filename= "xxx.stan"
    ; line_num= 35
    ; col_num= 24
    ; included_from=
        Some
          { filename= "yyy.stan"
          ; line_num= 345
          ; col_num= 214
          ; included_from= None } }
  in
  print_endline (string_of_location (parse_location (string_of_location loc))) ;
  [%expect
    {|
      file xxx.stan, line 35, column 24, included from
      file yyy.stan, line 345, column 214 |}]

let%expect_test "parse location from string" =
  let loc = parse_location "file xxx.stan, line 245, column 13" in
  print_endline loc.filename ;
  print_endline (string_of_int loc.line_num) ;
  print_endline (string_of_int loc.col_num) ;
  [%expect {|
      xxx.stan
      245
      13 |}]
