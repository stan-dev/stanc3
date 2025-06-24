(** Helper program to remove extraneous comments from Menhir's
    .messages file *)

let strip_redundant_parser_states content =
  let pattern =
    Str.regexp
      ("\\(\\(program\\|functions_only\\):.*\n\\)" ^ "\\(##.*\n\\)"
     ^ "\\(## Concrete.*\n\\)?" ^ "\\(##.*\n\\)+"
     ^ "\\(\\(program\\|functions_only\\):.*\n\\)") in
  Str.global_substitute pattern
    (fun s ->
      let group1 = Str.matched_group 1 s in
      let group4 = try Str.matched_group 4 s with Not_found -> "" in
      let group6 = Str.matched_group 6 s in
      group1 ^ group4 ^ group6)
    content

let strip_lines content =
  String.split_on_char '\n' content
  |> List.map (fun line ->
         let len = String.length line in
         let rec find_end i =
           if i < 0 then 0
           else if line.[i] = ' ' || line.[i] = '\t' then find_end (i - 1)
           else i + 1 in
         String.sub line 0 (find_end (len - 1)))
  |> String.concat "\n"

let rec strip_until_fixed content =
  let stripped = strip_redundant_parser_states content in
  if String.equal content stripped then stripped else strip_until_fixed stripped

let () =
  let content = In_channel.input_all In_channel.stdin in
  let result = strip_until_fixed content |> strip_lines in
  print_string result
