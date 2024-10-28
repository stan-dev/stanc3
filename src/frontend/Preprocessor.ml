(** Preprocessor for handling include directives *)

open Core
open Lexing
open Debugging

let comments = Queue.create ()
let add_comment = Queue.enqueue comments
let get_comments () = Queue.to_list comments
let include_stack = Stack.create ()
let included_files : string list ref = ref []
let size () = Stack.length include_stack

let locations_map : (string * Middle.Location.t option) String.Table.t =
  String.Table.create ()

let new_file_start_position filename included_from =
  (* Lexing.position does not have a field to store `included_from`
     so we store it in a global hashmap instead and put the hashmap key
     in `pos_fname`. The keys are arbitrary unique strings. (Filenames are
     not good keys because the same file could be included multiple times.)

     Prefixing the key with NUL allows us to do a little optimization:
     when `included_from` is None we don't need to access the hashmap and
     can store the filename directly in `pos_fname`. Filenames never
     begin with NUL so `location_of_position` only needs to check the
     first character to know whether to do a hashmap lookup.
  *)
  if Option.is_none included_from then
    {Lexing.pos_fname= filename; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}
  else
    let key = "\u{0}" ^ string_of_int (Hashtbl.length locations_map) in
    Hashtbl.add_exn locations_map ~key ~data:(filename, included_from);
    {Lexing.pos_fname= key; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}

let location_of_position {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  let filename, included_from =
    if Char.equal (String.get pos_fname 0) (Char.of_string "\u{0}") then
      Hashtbl.find_exn locations_map pos_fname
    else (pos_fname, None) in
  { Middle.Location.line_num= pos_lnum
  ; col_num= pos_cnum - pos_bol
  ; filename
  ; included_from }

let location_span_of_positions (start_pos, end_pos) : Middle.Location_span.t =
  { begin_loc= location_of_position start_pos
  ; end_loc= location_of_position end_pos }

let init buf filename =
  Hashtbl.clear locations_map;
  Queue.clear comments;
  included_files := [];
  Stack.clear include_stack;
  Stack.push include_stack buf;
  buf.lex_start_p <- new_file_start_position filename None;
  buf.lex_curr_p <- buf.lex_start_p

let current_buffer () =
  let buf = Stack.top_exn include_stack in
  buf

let current_location () =
  location_of_position (lexeme_start_p (current_buffer ()))

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
  lexer_logger "Switching to older lexbuf";
  lexer_pos_logger old_lexbuf.lex_curr_p;
  lexbuf.lex_curr_p <- old_pos;
  lexbuf.lex_start_p <- old_pos;
  old_lexbuf

let find_include_fs lookup_paths fname =
  let rec loop paths =
    match paths with
    | [] ->
        let message =
          let pp_list ppf l =
            match l with
            | [] -> Fmt.string ppf "None"
            | _ -> Fmt.(list ~sep:comma string) ppf l in
          Fmt.str
            "Could not find include file '%s' in specified include paths.@\n\
             @[Current include paths: %a@]" fname pp_list lookup_paths in
        raise (Errors.SyntaxError (Include (message, current_location ())))
    | path :: rest_of_paths -> (
        try
          let full_path = path ^ "/" ^ fname in
          (In_channel.create full_path |> Lexing.from_channel, full_path)
        with _ -> loop rest_of_paths) in
  loop lookup_paths

let find_include_inmemory map fname =
  match Map.find map fname with
  | None ->
      let message =
        let pp_list ppf l =
          let keys = Map.keys l in
          if List.is_empty keys then Fmt.string ppf "None"
          else Fmt.(list ~sep:comma string) ppf keys in
        Fmt.str
          "Could not find include file '%s'.@ stanc was given information \
           about the following files:@ %a"
          fname pp_list map in
      raise (Errors.SyntaxError (Include (message, current_location ())))
  | Some s -> (Lexing.from_string s, fname)

let find_include fname =
  match !Include_files.include_provider with
  | FileSystemPaths lookup_paths -> find_include_fs lookup_paths fname
  | InMemory map -> find_include_inmemory map fname

let try_get_new_lexbuf fname =
  let lexbuf = Stack.top_exn include_stack in
  let new_lexbuf, file = find_include fname in
  lexer_logger ("opened " ^ file);
  new_lexbuf.lex_start_p <-
    new_file_start_position file
    @@ Some (location_of_position lexbuf.lex_start_p);
  new_lexbuf.lex_curr_p <- new_lexbuf.lex_start_p;
  let dup_exists {Middle.Location.filename; included_from; _} =
    let is_dup = String.equal filename in
    let rec go = function
      | None -> false
      | Some {Middle.Location.filename; included_from; _} ->
          if is_dup filename then true else go included_from in
    go included_from in
  if dup_exists (location_of_position lexbuf.lex_start_p) then
    raise
      (Errors.SyntaxError
         (Include
            ( Printf.sprintf "File %s recursively included itself." fname
            , location_of_position (lexeme_start_p lexbuf) )));
  Stack.push include_stack new_lexbuf;
  update_start_positions new_lexbuf.lex_curr_p;
  included_files := file :: !included_files;
  new_lexbuf
