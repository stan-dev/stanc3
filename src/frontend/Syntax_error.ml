open Std

type styled_text = (unit, Format.formatter, unit) format

(** Our type of syntax error information *)
type err =
  | Lexing
  | UnexpectedEOF
  | Include of (string * string option)
  | Parsing of styled_text

type t = Middle.Location_span.t * err

(** Sets up the semantic tag machinery
    (https://ocaml.org/manual/api/Format.html#tags) to print ANSI escape codes
    for formatting *)
let pp_styled_text : styled_text Fmt.t =
 fun ppf format_string ->
  let ansi_stags former =
    let str_to_esc_seq styling =
      match String.lowercase_ascii styling with
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
        Stack.to_seq styles
        |> Seq.take_while (Fun.negate @@ String.equal "0")
        |> List.of_seq |> List.rev in
      let escs = String.concat ~sep:";" ("0" :: styles_until_reset) in
      Printf.sprintf "\027[%sm" escs in
    Format.
      { former with
        mark_open_stag=
          (function
          | String_tag s -> (
              match str_to_esc_seq s with
              | Some eseq ->
                  Stack.push eseq styles;
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

let to_grace ?printed_filename ?code ((loc, err) : t) =
  let open Grace.Diagnostic in
  let range, included =
    Diagnostic.range_of_loc_span ?printed_filename ?code loc in
  let primary = Label.primaryf ~range "here." in
  let summary, notes =
    match err with
    | Parsing message ->
        (* TODO: This is a simple way of splitting up the error messages from
           the parser for Grace. Ideally, we'd have the ability to return a
           richer type than just a string -- cf the [.messages_ml] suggestion in
           https://gitlab.inria.fr/fpottier/menhir/-/blob/master/TODO.md#enhancements,
           or the error format in LRGrep.

           This would let us do something more like Semantic_error, where we can
           more carefully chose what goes in the summary/primary label/notes. *)
        let message_str = Stdlib.string_of_format message in
        let summary, note =
          match String.split_first ~sep:".@} " message_str with
          | Some (first_sentence, rest) ->
              ( "Syntax error: "
                ^^ Scanf.format_from_string first_sentence ""
                ^^ ".@}"
              , Scanf.format_from_string rest "" )
          | None -> (message, message) in
        ( (fun ppf -> pp_styled_text ppf summary)
        , [(fun ppf -> pp_styled_text ppf note)] )
    | Include (message, note) ->
        ( Message.createf "%s@." message
        , Option.map ~f:Message.create note |> Option.to_list )
    | Lexing -> (Message.createf "Invalid character found.@.", [])
    | UnexpectedEOF -> (Message.createf "Unexpected end of input.@.", []) in
  create Error ~labels:(Diagnostic.unstyle primary :: included) ~notes summary

exception ParserException of styled_text * Middle.Location_span.t
exception UnexpectedEOF of Middle.Location_span.t
exception UnexpectedCharacter of Middle.Location_span.t
exception IncludeError of string * string option * Middle.Location_span.t

let unexpected_eof loc = raise (UnexpectedEOF loc)
let unexpected_character loc = raise (UnexpectedCharacter loc)
let include_error ?note msg loc = raise (IncludeError (msg, note, loc))
let parse_error msg loc = raise (ParserException (msg, loc))

let try_with f =
  try Ok (f ()) with
  | ParserException (msg, loc) -> Error (loc, Parsing msg)
  | UnexpectedEOF loc -> Error (loc, UnexpectedEOF)
  | UnexpectedCharacter loc -> Error (loc, Lexing)
  | IncludeError (msg, note, loc) -> Error (loc, Include (msg, note))

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
