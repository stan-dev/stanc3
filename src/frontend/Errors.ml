(** Setup of our compiler errors *)

open Core

type t =
  | FileNotFound of string
  | Syntax_error of Syntax_error.t
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

let red = Fmt.(styled `Bold (styled (`Fg `Red) string))

let pp ?printed_filename ?code ppf = function
  | FileNotFound f ->
      Fmt.pf ppf "%a: file '%s' not found or cannot be opened@." red "Error" f
  | Syntax_error err ->
      let loc_span = Syntax_error.location err in
      let error_type = Syntax_error.kind err in
      Fmt.pf ppf "%a in %a, %s:@;%a@,%a" red "Syntax error"
        (Middle.Location_span.pp ?printed_filename)
        loc_span error_type (pp_context ?code) loc_span.begin_loc
        Syntax_error.pp err
  | Semantic_error err ->
      let loc_span = Semantic_error.location err in
      Fmt.pf ppf "%a in %a:@;%a@,%a@." red "Semantic error"
        (Middle.Location_span.pp ?printed_filename)
        loc_span (pp_context ?code) loc_span.begin_loc Semantic_error.pp err
  | DebugDataError (loc, msg) ->
      if Middle.Location_span.(compare loc empty = 0) then
        Fmt.pf ppf "%a: %s" red "Error" msg
      else
        Fmt.pf ppf "@[<v>%a in %a:@ %s@;@]" red "Error"
          (Middle.Location_span.pp ?printed_filename)
          loc msg
