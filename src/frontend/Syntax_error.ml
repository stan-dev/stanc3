open Core

(** Our type of syntax error information *)
type t =
  | Lexing of Middle.Location_span.t
  | UnexpectedEOF of Middle.Location_span.t
  | Include of string * Middle.Location_span.t
  | Parsing of string * Middle.Location_span.t

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
let pp ppf = function
  | Parsing (message, _) -> markup_text ppf message
  | Lexing _ -> Fmt.pf ppf "Invalid character found.@."
  | UnexpectedEOF _ -> Fmt.pf ppf "Unexpected end of input.@."
  | Include (message, _) -> Fmt.pf ppf "%s@." message

exception ParserException of string * Middle.Location_span.t
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
