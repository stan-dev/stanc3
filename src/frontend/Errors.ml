(** Setup of our compiler errors *)

open Core

(** Our type of syntax error information *)
type syntax_error =
  | Lexing of Middle.Location.t
  | UnexpectedEOF of Middle.Location.t
  | Include of string * Middle.Location.t
  | Parsing of string * Middle.Location_span.t

(** Exception for Syntax Errors *)
exception SyntaxError of syntax_error

(** Exception [SemanticError (msg, loc)] indicates a semantic error with message
    [msg], occurring in location [loc]. *)
exception SemanticError of Semantic_error.t

type t =
  | FileNotFound of string
  | Syntax_error of syntax_error
  | Semantic_error of Semantic_error.t
  | DebugDataError of (Middle.Location_span.t * string)

let get_context ?code Middle.Location.{filename; included_from; _} =
  let lines () =
    (* If the location is not included from anywhere, and we
       have code provided, use it *)
    match (included_from, code) with
    | None, Some code -> String.split_lines code
    | _ -> (
        (* Otherwise, by the time we are printing an error,
           all these files are already resolved. *)
        match !Include_files.include_provider with
        | FileSystemPaths _ ->
            (* So we can read directly from the filesystem *)
            In_channel.read_lines filename
        | InMemory m ->
            (* Or, we know we can find it in the map *)
            String.split_lines (Map.find_exn m filename)) in
  Option.try_with lines

let pp_context ?code ppf loc =
  let context =
    get_context ?code loc
    |> Option.map ~f:(fun lines -> (loc, Array.of_list lines)) in
  (Fmt.option Middle.Location.pp_context_for) ppf context

let red = Fmt.(styled `Bold (styled (`Fg `Red) string))

let pp_semantic_error ?printed_filename ?code ppf err =
  let loc_span = Semantic_error.location err in
  Fmt.pf ppf "%a in %a:@;%a@,%a@." red "Semantic error"
    (Middle.Location_span.pp ?printed_filename)
    loc_span (pp_context ?code) loc_span.begin_loc Semantic_error.pp err

(** Sets up the semantic tag machinery (https://ocaml.org/manual/api/Format.html#tags)
   to print ANSI escape codes for formatting
*)
let styled_text ppf str =
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
  try
    let format_string =
      (* treat input like a format string with no placeholders *)
      Scanf.format_from_string str "" in
    (* Even though this isn't using the [Fmt.styled] system,
       we still use Fmt's style _setting_ for consistent behavior *)
    match Fmt.style_renderer ppf with
    | `None -> Fmt.fmt format_string ppf
    | `Ansi_tty ->
        let former = Format.pp_get_formatter_stag_functions ppf () in
        let marks = Format.pp_get_mark_tags ppf () in
        Format.pp_set_formatter_stag_functions ppf (ansi_stags former);
        Format.pp_set_mark_tags ppf true;
        Fun.protect
          (fun () ->
            Fmt.fmt format_string ppf;
            Fmt.flush ppf ())
          ~finally:(fun () ->
            Format.pp_set_formatter_stag_functions ppf former;
            Format.pp_set_mark_tags ppf marks)
  with Scanf.Scan_failure _ ->
    (* This means the runtime typecheck of str failed, usually because
       a format specifier like %d was used somewhere. We could throw a noisy
       error here, but for now I've chosen to just print the string
    *)
    Fmt.string ppf str

let%expect_test "colored output" =
  let s =
    "@{<b>This @{<red>does @{<blue>what @{<reset>y@{<i>@{<green>o@}@}u@}@} \
     want@}!@}" in
  Fmt.set_style_renderer Fmt.stdout `None;
  styled_text Fmt.stdout s;
  Format.pp_print_newline Fmt.stdout ();
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  styled_text Fmt.stdout s;
  Format.pp_print_newline Fmt.stdout ();
  styled_text Fmt.stdout (s ^ "%d");
  [%expect
    {|
    This does what you want!
    [0;1mThis [0;1;31mdoes [0;1;31;34mwhat [0my[0;3m[0;3;32mo[0;3m[0mu[0;1;31;34m[0;1;31m want[0;1m![0m
    @{<b>This @{<red>does @{<blue>what @{<reset>y@{<i>@{<green>o@}@}u@}@} want@}!@}%d |}]

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ?printed_filename ?code ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "%a in %a, parsing error:@,%a@,%a" red "Syntax error"
        (Middle.Location_span.pp ?printed_filename)
        loc_span (pp_context ?code) loc_span.begin_loc styled_text message
  | Lexing loc ->
      Fmt.pf ppf "%a in %a, lexing error:@,%a@,%s@." red "Syntax error"
        (Middle.Location.pp ?printed_filename ())
        {loc with col_num= loc.col_num - 1}
        (pp_context ?code) loc "Invalid character found."
  | UnexpectedEOF loc ->
      Fmt.pf ppf "%a in %a, lexing error:@,%a@,%s@." red "Syntax error"
        (Middle.Location.pp ?printed_filename ())
        {loc with col_num= loc.col_num - 1}
        (pp_context ?code) loc "Unexpected end of input"
  | Include (message, loc) ->
      Fmt.pf ppf "%a in %a, include error:@,%a@,%s@." red "Syntax error"
        (Middle.Location.pp ?printed_filename ())
        loc (pp_context ?code) loc message

let pp ?printed_filename ?code ppf = function
  | FileNotFound f ->
      Fmt.pf ppf "%a: file '%s' not found or cannot be opened@." red "Error" f
  | Syntax_error e -> pp_syntax_error ?printed_filename ?code ppf e
  | Semantic_error e -> pp_semantic_error ?printed_filename ?code ppf e
  | DebugDataError (loc, msg) ->
      if Middle.Location_span.(compare loc empty = 0) then
        Fmt.pf ppf "%a: %s" red "Error" msg
      else
        Fmt.pf ppf "@[<v>%a in %a:@ %s@;@]" red "Error"
          (Middle.Location_span.pp ?printed_filename)
          loc msg
