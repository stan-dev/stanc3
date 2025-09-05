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
  Option.try_with @@ fun () ->
  match (included_from, code) with
  | None, Some code ->
      (* If the location is not included from anywhere, and we
         have code provided, use it *)
      String.split_lines code
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
  let context =
    get_context ?code loc
    |> Option.map ~f:(fun lines -> (loc, Array.of_list lines)) in
  (Fmt.option Middle.Location.pp_context_for) ppf context

let pp_semantic_error ?printed_filename ?code ppf err =
  let loc_span = Semantic_error.location err in
  Fmt.pf ppf "Semantic error in %a:@;%a@,%a@."
    (Middle.Location_span.pp ?printed_filename)
    loc_span (pp_context ?code) loc_span.begin_loc Semantic_error.pp err

(** A syntax error message used when handling a SyntaxError *)
let pp_syntax_error ?printed_filename ?code ppf = function
  | Parsing (message, loc_span) ->
      Fmt.pf ppf "Syntax error in %a, parsing error:@,%a@,%s"
        (Middle.Location_span.pp ?printed_filename)
        loc_span (pp_context ?code) loc_span.begin_loc message
  | Lexing loc ->
      Fmt.pf ppf "Syntax error in %a, lexing error:@,%a@,%s@."
        (Middle.Location.pp printed_filename)
        {loc with col_num= loc.col_num - 1}
        (pp_context ?code) loc "Invalid character found."
  | UnexpectedEOF loc ->
      Fmt.pf ppf "Syntax error in %a, lexing error:@,%a@,%s@."
        (Middle.Location.pp printed_filename)
        {loc with col_num= loc.col_num - 1}
        (pp_context ?code) loc "Unexpected end of input"
  | Include (message, loc) ->
      Fmt.pf ppf "Syntax error in %a, include error:@,%a@,%s@."
        (Middle.Location.pp printed_filename)
        loc (pp_context ?code) loc message

let pp ?printed_filename ?code ppf = function
  | FileNotFound f ->
      Fmt.pf ppf "Error: file '%s' not found or cannot be opened@." f
  | Syntax_error e -> pp_syntax_error ?printed_filename ?code ppf e
  | Semantic_error e -> pp_semantic_error ?printed_filename ?code ppf e
  | DebugDataError (loc, msg) ->
      if Middle.Location_span.(compare loc empty = 0) then
        Fmt.pf ppf "Error: %s" msg
      else
        Fmt.pf ppf "@[<v>Error in %a:@ %s@;@]"
          (Middle.Location_span.pp ?printed_filename)
          loc msg
