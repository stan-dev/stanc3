(** Helper program to remove redundant comments from Menhir's .messages file.
    For messages that are used by multiple different parser states, all of the
    comment lines except for the 'concrete syntax' line are elided. *)

open Std

let strip_redundant_parser_states content =
  let pattern =
    (* match comment blocks between two input sentences *)
    let input_sentence_p = {|\(\(program\|functions_only\):.*\)|} in
    Str.regexp
      (input_sentence_p ^ {|
##
\(## Concrete syntax: .*\)
\(##.*
\)+|}
     ^ input_sentence_p) in
  (* preserve only the input sentences and the "Concrete Syntax" line *)
  Str.global_replace pattern "\\1\n\\3\n\\5" content

let rtrim =
  String.drop_last_while (function ' ' | '\n' | '\r' -> true | _ -> false)

let strip_lines content =
  String.split_all ~sep:"\n" content
  |> List.map ~f:rtrim |> String.concat ~sep:"\n"

let rec strip_until_fixed content =
  let stripped = strip_redundant_parser_states content in
  if String.equal content stripped then stripped else strip_until_fixed stripped

let () =
  let content = In_channel.input_all In_channel.stdin in
  let result = strip_until_fixed content |> strip_lines in
  print_string result
