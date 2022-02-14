(** Preprocessor for handling include directives *)

open Core_kernel
open Lexing
open Debugging
module Str = Re.Str

let dup_exists l =
  match List.find_a_dup ~compare:String.compare l with
  | Some _ -> true
  | None -> false

let include_stack = Stack.create ()
let include_paths : string list ref = ref []
let included_files : string list ref = ref []
let size () = Stack.length include_stack

let init buf =
  included_files := [] ;
  Stack.clear include_stack ;
  Stack.push include_stack buf

let current_buffer () =
  let buf = Stack.top_exn include_stack in
  buf

let pop_buffer () = Stack.pop_exn include_stack

let update_start_positions pos =
  Stack.iter ~f:(fun lexbuf -> lexbuf.lex_start_p <- pos) include_stack

let restore_prior_lexbuf () =
  let lexbuf = pop_buffer () in
  let old_lexbuf = current_buffer () in
  (* to get printing includes right we need to make sure that the 'start' of
      our next token is on the following line
  *)
  let old_pos =
    {old_lexbuf.lex_curr_p with pos_lnum= old_lexbuf.lex_curr_p.pos_lnum + 1}
  in
  lexer_logger "Switching to older lexbuf" ;
  lexer_pos_logger old_lexbuf.lex_curr_p ;
  lexbuf.lex_curr_p <- old_pos ;
  lexbuf.lex_start_p <- old_pos ;
  old_lexbuf

let rec try_open_in paths fname pos =
  match paths with
  | [] ->
      raise
        (Errors.SyntaxError
           (Include
              ( "Could not find include file " ^ fname
                ^ " in specified include paths."
              , Middle.Location.of_position_exn
                  (lexeme_start_p (Stack.top_exn include_stack)) ) ) )
  | path :: rest_of_paths -> (
    try
      let full_path = path ^ "/" ^ fname in
      ( In_channel.create full_path
      , full_path
      , sprintf "%s, included from\n%s" full_path
          (Middle.Location.to_string
             (Middle.Location.of_position_exn
                (Stack.top_exn include_stack).lex_start_p ) ) )
    with _ -> try_open_in rest_of_paths fname pos )

let maybe_remove_quotes str =
  let open String in
  if is_prefix str ~prefix:"\"" && is_suffix str ~suffix:"\"" then
    drop_suffix (drop_prefix str 1) 1
  else str

let try_get_new_lexbuf fname =
  let lexbuf = Stack.top_exn include_stack in
  let pos = lexbuf.lex_curr_p in
  let chan, file, path =
    try_open_in !include_paths (maybe_remove_quotes fname) pos in
  lexer_logger ("opened " ^ file) ;
  let new_lexbuf = from_channel chan in
  new_lexbuf.lex_start_p <-
    {pos_fname= path; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
  new_lexbuf.lex_curr_p <- new_lexbuf.lex_start_p ;
  if dup_exists (Str.split (Str.regexp ", included from\n") path) then
    raise
      (Errors.SyntaxError
         (Include
            ( Printf.sprintf "File %s recursively included itself." fname
            , Middle.Location.of_position_exn (lexeme_start_p lexbuf) ) ) ) ;
  Stack.push include_stack new_lexbuf ;
  update_start_positions new_lexbuf.lex_curr_p ;
  included_files := file :: !included_files ;
  new_lexbuf
