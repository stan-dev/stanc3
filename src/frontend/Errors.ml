(** Setup of our compiler errors *)

open! Std
open Diagnostic

type t =
  | FileNotFound of string
  | Syntax_error of Syntax_error.t
  | Semantic_error of Semantic_error.t
  | DebugDataError of (Middle.Location_span.t * string * bool)

let to_grace ?printed_filename ?code t =
  let open Grace.Diagnostic in
  match t with
  | FileNotFound f -> createf Error "file '%s' not found or cannot be opened" f
  | Syntax_error err -> Syntax_error.to_grace ?printed_filename ?code err
  | Semantic_error err -> Semantic_error.to_grace ?printed_filename ?code err
  | DebugDataError (loc, msg, had_context) ->
      let notes =
        if had_context then []
        else [Message.create "Supplying a --debug-data-file may help."] in
      let labels =
        if Middle.Location_span.(compare loc empty = 0) then []
        else
          let range, included = range_of_loc_span ?printed_filename ?code loc in
          Label.primaryf ~range "here" :: included in
      createf ~labels ~notes Error "%s" msg

let pp ?printed_filename ?code ppf t =
  let diagnostic = to_grace ?printed_filename ?code t in
  Fmt.pf ppf "%a@." Diagnostic.pp diagnostic
