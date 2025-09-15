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

module Tests = struct
  (** tip: view this file using `cat` to see the styling in the test output *)

  let%expect_test "nested formatting" =
    let s : _ format =
      "@{<b>This @{<red>does @{<blue>what @{<r>y@{<i>@{<green>o@}@}u@}@} \
       want@}!@}" in
    Fmt.set_style_renderer Fmt.stdout `None;
    pp_styled_text Fmt.stdout s;
    Format.pp_print_newline Fmt.stdout ();
    Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
    pp_styled_text Fmt.stdout s;
    [%expect
      {|
    This does what you want!
    [0;1mThis [0;1;31mdoes [0;1;31;34mwhat [0my[0;3m[0;3;32mo[0;3m[0mu[0;1;31;34m[0;1;31m want[0;1m![0m |}]

  let%expect_test "formatting with stags" =
    let s : _ format =
      {|
    @{<bold>bold@}
    @{<italic>italic@}
    @{<underline>underline@}
    @{<faint>faint@}
    @{<reset>reset@}
    @{<reverse>reverse@}
    @{<black>black@}
    @{<red>red@}
    @{<green>green@}
    @{<yellow>yellow@}
    @{<blue>blue@}
    @{<magenta>magenta@}
    @{<cyan>cyan@}
    @{<white>white@}
    @{<bg_black>bg_black@}
    @{<bg_red>bg_red@}
    @{<bg_green>bg_green@}
    @{<bg_yellow>bg_yellow@}
    @{<bg_blue>bg_blue@}
    @{<bg_magenta>bg_magenta@}
    @{<bg_cyan>bg_cyan@}
    @{<bg_white>bg_white@}
    @{<light_black>light_black@}
    @{<light_red>light_red@}
    @{<light_green>light_green@}
    @{<light_yellow>light_yellow@}
    @{<light_blue>light_blue@}
    @{<light_magenta>light_magenta@}
    @{<light_cyan>light_cyan@}
    @{<light_white>light_white@}
    @{<body>Unknown tag@}|}
    in
    Fmt.set_style_renderer Fmt.stdout `None;
    pp_styled_text Fmt.stdout s;
    Format.pp_print_newline Fmt.stdout ();
    Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
    pp_styled_text Fmt.stdout s;
    [%expect
      {|
    bold
    italic
    underline
    faint
    reset
    reverse
    black
    red
    green
    yellow
    blue
    magenta
    cyan
    white
    bg_black
    bg_red
    bg_green
    bg_yellow
    bg_blue
    bg_magenta
    bg_cyan
    bg_white
    light_black
    light_red
    light_green
    light_yellow
    light_blue
    light_magenta
    light_cyan
    light_white
    Unknown tag

    [0;1mbold[0m
    [0;3mitalic[0m
    [0;4munderline[0m
    [0;2mfaint[0m
    [0mreset[0m
    [0;7mreverse[0m
    [0;30mblack[0m
    [0;31mred[0m
    [0;32mgreen[0m
    [0;33myellow[0m
    [0;34mblue[0m
    [0;35mmagenta[0m
    [0;36mcyan[0m
    [0;37mwhite[0m
    [0;40mbg_black[0m
    [0;41mbg_red[0m
    [0;42mbg_green[0m
    [0;43mbg_yellow[0m
    [0;44mbg_blue[0m
    [0;45mbg_magenta[0m
    [0;46mbg_cyan[0m
    [0;47mbg_white[0m
    [0;90mlight_black[0m
    [0;91mlight_red[0m
    [0;92mlight_green[0m
    [0;93mlight_yellow[0m
    [0;94mlight_blue[0m
    [0;95mlight_magenta[0m
    [0;96mlight_cyan[0m
    [0;97mlight_white[0m
    <body>Unknown tag</body> |}]
end
