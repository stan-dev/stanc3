(** Setup of our compiler errors *)

open Core_kernel
open Ast

(** Our type of syntax error information *)
type parse_error =
  | Lexing of string * location
  | Includes of string * location
  | Parsing of string * Ast.location_span

(** Exception for Syntax Errors *)
exception SyntaxError of parse_error

(** Exception [SemanticError (msg, loc)] indicates a semantic error with message
    [msg], occurring at location [loc]. *)
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
let rec parse_location_from_string str =
  let split_str =
    Str.bounded_split
      (Str.regexp "file \\|, line \\|, column \\|, included from\n")
      str 4
  in
  match split_str with
  | [fname; linenum; colnum] ->
      { filename= fname
      ; linenum= int_of_string linenum
      ; colnum= int_of_string colnum
      ; included_from= None }
  | [fname; linenum_str; colnum_str; included_from_str] ->
      { filename= fname
      ; linenum= int_of_string linenum_str
      ; colnum= int_of_string colnum_str
      ; included_from= Some (parse_location_from_string included_from_str) }
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
          ; linenum= pos_lnum
          ; colnum= pos_cnum - pos_bol
          ; included_from=
              ( match fnames with
              | [] -> None
              | fnames :: _ -> Some (parse_location_from_string fnames) ) } )

(** Take the AST.location_span corresponding to a pair of Lexing.position's *)
let location_span_of_pos start_pos end_pos =
  { start_loc= location_of_position start_pos
  ; end_loc= location_of_position end_pos }

(** Render a location as a string *)
let rec create_string_from_location loc =
  let open Format in
  let included_from_str =
    match loc.included_from with
    | None -> ""
    | Some loc2 ->
        sprintf ", included from\n%s" (create_string_from_location loc2)
  in
  sprintf "file %s, line %d, column %d%s" loc.filename loc.linenum loc.colnum
    included_from_str

(** Render a location_span as a string *)
let create_string_from_location_span loc_sp =
  match loc_sp with {start_loc; end_loc} ->
    let open Format in
    let filename_str = sprintf "file %s, " start_loc.filename in
    let linenum_str =
      if end_loc.linenum = start_loc.linenum then
        sprintf "line %d, " end_loc.linenum
      else sprintf "lines %d-%d, " start_loc.linenum end_loc.linenum
    in
    let colnum_str =
      if end_loc.linenum = start_loc.linenum then
        sprintf "columns %d-%d" start_loc.colnum end_loc.colnum
      else sprintf "column %d" start_loc.colnum
    in
    let included_from_str =
      match start_loc.included_from with
      | None -> ""
      | Some loc ->
          sprintf ", included from\n%s" (create_string_from_location loc)
    in
    sprintf "%s%s%s%s" filename_str linenum_str colnum_str included_from_str

(** We quote the lines before and after a certain location in the program *)
let print_context {filename; linenum; colnum; _} =
  try
    let open In_channel in
    let input = create filename in
    for _ = 1 to linenum - 3 do
      ignore (input_line_exn input)
    done ;
    let open Printf in
    let line_2_before =
      if linenum > 2 then
        sprintf "%6d:  %s\n" (linenum - 2) (input_line_exn input)
      else ""
    in
    let line_before =
      if linenum > 1 then
        sprintf "%6d:  %s\n" (linenum - 1) (input_line_exn input)
      else ""
    in
    let our_line = sprintf "%6d:  %s\n" linenum (input_line_exn input) in
    let cursor_line = String.make (colnum + 9) ' ' ^ "^\n" in
    let line_after =
      try sprintf "%6d:  %s\n" (linenum + 1) (input_line_exn input) with _ ->
        ""
    in
    let line_2_after =
      try sprintf "%6d:  %s\n" (linenum + 2) (input_line_exn input) with _ ->
        ""
    in
    close input ;
    Some
      (sprintf
         "   -------------------------------------------------\n\
          %s%s%s%s%s%s   -------------------------------------------------\n"
         line_2_before line_before our_line cursor_line line_after line_2_after)
  with _ -> None

(** We quote the lines before and after a certain location in the program
    and print a message *)
let print_context_and_message message loc =
  ( match print_context loc with
  | None -> ()
  | Some line -> Printf.eprintf "%s\n" line ) ;
  Printf.eprintf "%s\n\n" message

(** A syntax error message used when handling a SyntaxError *)
let report_syntax_error = function
  | Parsing (message, loc_span) ->
      Printf.eprintf "\nSyntax error at %s, parsing error:\n"
        (create_string_from_location_span loc_span) ;
      print_context_and_message message loc_span.end_loc
  | Lexing (_, loc) ->
      Printf.eprintf "\nSyntax error at %s, lexing error:\n"
        (create_string_from_location {loc with colnum= loc.colnum - 1}) ;
      print_context_and_message "Invalid character found." loc
  | Includes (message, loc) ->
      Printf.eprintf "\nSyntax error at %s, includes error:\n"
        (create_string_from_location loc) ;
      print_context_and_message message loc

(** A semantic error message used when handling a SemanticError *)
let report_semantic_error (message, loc_span) =
  Printf.eprintf "\n%s at %s:\n" "Semantic error"
    (create_string_from_location_span loc_span) ;
  print_context_and_message message loc_span.start_loc

(* Warn that a language feature is deprecated *)
let warn_deprecated (pos, message) =
  let loc =
    location_of_position {pos with Lexing.pos_cnum= pos.Lexing.pos_cnum - 1}
  in
  Printf.eprintf "\nWarning: deprecated language construct used at %s:\n"
    (create_string_from_location loc) ;
  print_context_and_message message loc

(* TESTS *)
let%expect_test "location string equivalence 1" =
  let str =
    "file xxx.stan, line 245, column 13, included from\n\
     file yyy.stan, line 666, column 42, included from\n\
     file zzz.stan, line 24, column 77"
  in
  print_endline (create_string_from_location (parse_location_from_string str)) ;
  [%expect
    {|
      file xxx.stan, line 245, column 13, included from
      file yyy.stan, line 666, column 42, included from
      file zzz.stan, line 24, column 77 |}]

let%expect_test "location string equivalence 2" =
  let loc =
    { filename= "xxx.stan"
    ; linenum= 35
    ; colnum= 24
    ; included_from=
        Some
          {filename= "yyy.stan"; linenum= 345; colnum= 214; included_from= None}
    }
  in
  print_endline
    (create_string_from_location
       (parse_location_from_string (create_string_from_location loc))) ;
  [%expect
    {|
      file xxx.stan, line 35, column 24, included from
      file yyy.stan, line 345, column 214 |}]

let%expect_test "parse location from string" =
  let loc = parse_location_from_string "file xxx.stan, line 245, column 13" in
  print_endline loc.filename ;
  print_endline (string_of_int loc.linenum) ;
  print_endline (string_of_int loc.colnum) ;
  [%expect {|
      xxx.stan
      245
      13 |}]
