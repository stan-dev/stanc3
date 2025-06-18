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

let get_code_lines ?code Middle.Location.{filename; included_from; _} () =
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
          String.split_lines (Map.find_exn m filename))

let pp_context ?code ppf loc =
  Middle.Location.pp_context_for (get_code_lines ?code loc) ppf loc

let red = Fmt.(styled `Bold (styled (`Fg `Red) string))

let pp_semantic_error ?printed_filename ?code ppf err =
  let loc_span = Semantic_error.location err in
  Fmt.pf ppf "%a in %a:@;%a@,%a@." red "Semantic error"
    (Middle.Location_span.pp ?printed_filename)
    loc_span (pp_context ?code) loc_span.begin_loc Semantic_error.pp err

let style_of_string : string -> Fmt.style option = function
  | "b" | "bold" -> Some `Bold
  | "i" -> Some `Italic
  | "u" -> Some `Underline
  | "f" | "faint" -> Some `Faint
  | "r" | "reset" -> Some `None
  | "reverse" -> Some `Reverse
  | "black" -> Some (`Fg `Black)
  | "cyan" -> Some (`Fg `Cyan)
  | "green" -> Some (`Fg `Green)
  | "white" -> Some (`Fg `White)
  | "yellow" -> Some (`Fg `Yellow)
  | "red" -> Some (`Fg `Red)
  | "blue" -> Some (`Fg `Blue)
  | "magenta" -> Some (`Fg `Magenta)
  | "bg_black" -> Some (`Bg `Black)
  | "bg_cyan" -> Some (`Bg `Cyan)
  | "bg_green" -> Some (`Bg `Green)
  | "bg_white" -> Some (`Bg `White)
  | "bg_yellow" -> Some (`Bg `Yellow)
  | "bg_red" -> Some (`Bg `Red)
  | "bg_blue" -> Some (`Bg `Blue)
  | "bg_magenta" -> Some (`Bg `Magenta)
  | "light_black" -> Some (`Fg (`Hi `Black))
  | "light_cyan" -> Some (`Fg (`Hi `Cyan))
  | "light_green" -> Some (`Fg (`Hi `Green))
  | "light_white" -> Some (`Fg (`Hi `White))
  | "light_yellow" -> Some (`Fg (`Hi `Yellow))
  | "light_red" -> Some (`Fg (`Hi `Red))
  | "light_blue" -> Some (`Fg (`Hi `Blue))
  | "light_magenta" -> Some (`Fg (`Hi `Magenta))
  | _ -> None

(** This function formats strings that contain styling directives using the
  [Fmt.styled] system.
  For example, the string ["$(red,Hello) $(b,world)!"] will be formatted
  with "Hello" in red and "world" in bold, followed by an
  exclamation mark. *)
let markup_text ppf s =
  let max_i = String.length s - 1 in
  let buf = Buffer.create (String.length s) in
  let styles = Stack.create () in
  let print_current_style () =
    let with_style =
      Stack.fold
        ~f:(fun pp style -> Fmt.styled style pp)
        ~init:Fmt.string styles in
    with_style ppf (Buffer.contents buf);
    Buffer.clear buf in
  let start_style s =
    print_current_style ();
    Stack.push styles s in
  let end_style () =
    print_current_style ();
    Stack.pop styles |> ignore in
  let rec loop i =
    let next = i + 1 in
    let continue () =
      Buffer.add_char buf s.[i];
      loop next in
    if i > max_i then print_current_style ()
    else
      match s.[i] with
      | '\\' ->
          Buffer.add_char buf s.[i];
          if next > max_i then loop next
          else (
            (* very simplistic escaping *)
            Buffer.add_char buf s.[next];
            loop (next + 1))
      | ')' when not (Stack.is_empty styles) ->
          end_style ();
          loop (i + 1)
      | '$'
        when (* only process if it's possible to
                have a style given the length *)
             i + 3 < max_i -> (
          let style_start = next + 1 in
          match (s.[next], String.index_from s style_start ',') with
          | '(', Some endi -> (
              let style_string =
                String.sub s ~pos:style_start ~len:(endi - style_start) in
              match style_of_string style_string with
              | Some style ->
                  start_style style;
                  loop (endi + 1)
              | None -> continue ())
          | _ -> continue ())
      | _ -> continue () in
  loop 0

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ?printed_filename ?code ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "%a in %a, parsing error:@,%a@,%a" red "Syntax error"
        (Middle.Location_span.pp ?printed_filename)
        loc_span (pp_context ?code) loc_span.begin_loc markup_text message
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
