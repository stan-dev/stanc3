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

let get_context ?code Middle.Location.{filename; included_from; _} () =
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

let pp_context_with_message ?code pp ppf (err, loc) =
  Fmt.pf ppf "%a@,%a" Middle.Location.pp_context
    (get_context ?code loc, loc)
    pp err

let red = Fmt.(styled `Bold (styled (`Fg `Red) string))

let pp_semantic_error ?printed_filename ?code ppf err =
  let loc_span = Semantic_error.location err in
  Fmt.pf ppf "%a in %a:@;%a@." red "Semantic error"
    (Middle.Location_span.pp ?printed_filename)
    loc_span
    (pp_context_with_message ?code Semantic_error.pp)
    (err, loc_span.begin_loc)

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ?printed_filename ?code ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "%a in %a, parsing error:@,%a" red "Syntax error"
        (Middle.Location_span.pp ?printed_filename)
        loc_span
        (pp_context_with_message ?code Fmt.string)
        (message, loc_span.begin_loc)
  | Lexing loc ->
      Fmt.pf ppf "%a in %a, lexing error:@,%a@." red "Syntax error"
        (Middle.Location.pp ?printed_filename ())
        {loc with col_num= loc.col_num - 1}
        (pp_context_with_message ?code Fmt.string)
        ("Invalid character found.", loc)
  | UnexpectedEOF loc ->
      Fmt.pf ppf "%a in %a, lexing error:@,%a@." red "Syntax error"
        (Middle.Location.pp ?printed_filename ())
        {loc with col_num= loc.col_num - 1}
        (pp_context_with_message ?code Fmt.string)
        ("Unexpected end of input", loc)
  | Include (message, loc) ->
      Fmt.pf ppf "%a in %a, include error:@,%a@." red "Syntax error"
        (Middle.Location.pp ?printed_filename ())
        loc
        (pp_context_with_message ?code Fmt.string)
        (message, loc)

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
