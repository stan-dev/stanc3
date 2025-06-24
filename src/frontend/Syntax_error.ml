open Core

type styled_text = (unit, Format.formatter, unit) format

(** Our type of syntax error information *)
type t =
  | Lexing of Middle.Location_span.t
  | UnexpectedEOF of Middle.Location_span.t
  | Include of string * Middle.Location_span.t
  | Parsing of styled_text * Middle.Location_span.t

let location = function
  | Parsing (_, loc_span)
   |Lexing loc_span
   |UnexpectedEOF loc_span
   |Include (_, loc_span) ->
      loc_span

let kind = function
  | Parsing _ -> "parsing error"
  | UnexpectedEOF _ | Lexing _ -> "lexing error"
  | Include _ -> "include error"

(** Sets up the semantic tag machinery (https://ocaml.org/manual/api/Format.html#tags)
   to print ANSI escape codes for formatting
*)
let pp_styled_text : styled_text Fmt.t =
 fun ppf format_string ->
  let ansi_stags former =
    let str_to_esc_seq styling =
      match String.lowercase styling with
      | "b" | "bold" -> Some "1"
      | "i" | "italic" -> Some "3"
      | "u" | "underline" -> Some "4"
      | "f" | "faint" -> Some "2"
      | "r" | "reset" -> Some "0"
      | "reverse" -> Some "7"
      | "black" -> Some "30"
      | "red" -> Some "31"
      | "green" -> Some "32"
      | "yellow" -> Some "33"
      | "blue" -> Some "34"
      | "magenta" -> Some "35"
      | "cyan" -> Some "36"
      | "white" -> Some "37"
      | "bg_black" -> Some "40"
      | "bg_red" -> Some "41"
      | "bg_green" -> Some "42"
      | "bg_yellow" -> Some "43"
      | "bg_blue" -> Some "44"
      | "bg_magenta" -> Some "45"
      | "bg_cyan" -> Some "46"
      | "bg_white" -> Some "47"
      | "light_black" -> Some "90"
      | "light_red" -> Some "91"
      | "light_green" -> Some "92"
      | "light_yellow" -> Some "93"
      | "light_blue" -> Some "94"
      | "light_magenta" -> Some "95"
      | "light_cyan" -> Some "96"
      | "light_white" -> Some "97"
      | _ -> None in
    let styles = Stack.create () in
    let print_current_styles () =
      let styles_until_reset =
        Stack.to_list styles
        |> List.take_while ~f:(function "0" -> false | _ -> true)
        |> List.rev in
      let escs =
        List.fold styles_until_reset ~init:"0" ~f:(fun acc s ->
            Printf.sprintf "%s;%s" acc s) in
      sprintf "\027[%sm" escs in
    Format.
      { former with
        mark_open_stag=
          (function
          | String_tag s -> (
              match str_to_esc_seq s with
              | Some eseq ->
                  Stack.push styles eseq;
                  print_current_styles ()
              | None -> former.mark_open_stag (String_tag s))
          | stag -> former.mark_open_stag stag)
      ; mark_close_stag=
          (function
          | String_tag s when Option.is_some (str_to_esc_seq s) ->
              Stack.pop styles |> ignore;
              print_current_styles ()
          | stag -> former.mark_close_stag stag) } in
  match Fmt.style_renderer ppf with
  | `None -> Fmt.pf ppf format_string
  | `Ansi_tty ->
      let former = Format.pp_get_formatter_stag_functions ppf () in
      let marks = Format.pp_get_mark_tags ppf () in
      Format.pp_set_formatter_stag_functions ppf (ansi_stags former);
      Format.pp_set_mark_tags ppf true;
      Fun.protect
        (fun () ->
          Fmt.pf ppf format_string;
          Fmt.flush ppf ())
        ~finally:(fun () ->
          Format.pp_set_formatter_stag_functions ppf former;
          Format.pp_set_mark_tags ppf marks)

let%expect_test "colored output" =
  let s : _ format =
    "@{<b>This @{<red>does @{<blue>what @{<reset>y@{<i>@{<green>o@}@}u@}@} \
     want@}!@}" in
  Fmt.set_style_renderer Fmt.stdout `None;
  pp_styled_text Fmt.stdout s;
  Format.pp_print_newline Fmt.stdout ();
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  pp_styled_text Fmt.stdout s;
  (* tip: view this file using `cat` to see the styling in the test output *)
  [%expect
    {|
    This does what you want!
    [0;1mThis [0;1;31mdoes [0;1;31;34mwhat [0my[0;3m[0;3;32mo[0;3m[0mu[0;1;31;34m[0;1;31m want[0;1m![0m |}]

let pp ppf = function
  | Parsing (message, _) -> pp_styled_text ppf message
  | Lexing _ -> Fmt.pf ppf "Invalid character found.@."
  | UnexpectedEOF _ -> Fmt.pf ppf "Unexpected end of input.@."
  | Include (message, _) -> Fmt.pf ppf "%s@." message

exception ParserException of styled_text * Middle.Location_span.t
exception UnexpectedEOF of Middle.Location_span.t
exception UnexpectedCharacter of Middle.Location_span.t
exception IncludeError of string * Middle.Location_span.t

let unexpected_eof loc = raise (UnexpectedEOF loc)
let unexpected_character loc = raise (UnexpectedCharacter loc)
let include_error msg loc = raise (IncludeError (msg, loc))
let parse_error msg loc = raise (ParserException (msg, loc))

let try_with f =
  try Ok (f ()) with
  | ParserException (msg, loc) -> Error (Parsing (msg, loc))
  | UnexpectedEOF loc -> Error (UnexpectedEOF loc)
  | UnexpectedCharacter loc -> Error (Lexing loc)
  | IncludeError (msg, loc) -> Error (Include (msg, loc))
