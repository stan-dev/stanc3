(** Helper program to merge new error states to Menhir's .messages
   file *)

(** All the updates ultimately come from the same file,
   so we only need to read it once. *)
let cached_lines =
  let file_contents = ref None in
  fun file ->
    match !file_contents with
    | None ->
        let lines =
          In_channel.input_all (In_channel.open_text file)
          |> String.split_on_char '\n' |> Array.of_list in
        file_contents := Some lines;
        lines
    | Some lines -> lines

let copy_lines file line_no =
  let lines = cached_lines file in
  print_endline (Array.get lines (line_no - 1));
  let rec loop i =
    if i < Array.length lines then
      let line = Array.get lines i in
      if String.starts_with ~prefix:"##" line then (
        print_endline line;
        loop (i + 1)) in
  loop line_no

let update_one_message (new_file, line_no) =
  print_string "\n";
  copy_lines new_file line_no;
  print_endline "\nTODO: PARSER MESSAGE NEEDED HERE."

let parse_regex =
  Str.regexp
    {|File "\([^"]*\)", line \([0-9]+\).*
Error: this sentence.*
No sentence that leads to this state exists|}

let do_updates input_text =
  let rec find count pos =
    try
      let _ = Str.search_forward parse_regex input_text pos in
      let file = Str.matched_group 1 input_text in
      let line_no = int_of_string (Str.matched_group 2 input_text) in
      let next_pos = Str.match_end () in
      update_one_message (file, line_no);
      find (count + 1) next_pos
    with Not_found -> count in
  find 0 0

let () =
  let input_text = In_channel.input_all In_channel.stdin in
  let updates = do_updates input_text in
  Printf.eprintf "Added %d message stubs.\n" updates
