open Std
open Grace

let get_context code Middle.Location.{filename; included_from; _} =
  match (included_from, code) with
  | None, Some code ->
      (* If the location is not included from anywhere, and we have code
         provided, use it *)
      code
  | _ -> (
      (* Otherwise, by the time we are printing an error, all these files are
         already resolved. *)
      match !Include_files.include_provider with
      | FileSystemPaths _ ->
          (* So we can read directly from the filesystem *)
          In_channel.with_open_bin filename In_channel.input_all
      | InMemory m ->
          (* Or, we know we can find it in the map *)
          String.Map.find filename m)

let get_source ?printed_filename ?code loc =
  let code = get_context code loc in
  let name =
    if Option.is_none loc.included_from then
      Option.first_some printed_filename (Some loc.filename)
    else Some loc.filename in
  let content =
    (* Tab alignment hack, c.f. https://github.com/johnyob/grace/issues/83 *)
    String.replace_all ~sub:"\t" ~by:" " code in
  let source : Source.t = `String {name; content} in
  source

let clamp_int ~min ~max i = if i < min then min else if i > max then max else i

(** Generate secondary diagnostics identifying where a file was included *)
let rec included_diagnostic ?printed_filename ?code
    Middle.Location.{included_from; filename; _} : Diagnostic.Label.t list =
  let range_of_loc loc =
    let source = get_source ?printed_filename ?code loc in
    let start = loc.byte_num in
    let end_ =
      start + 8
      (* '#include' *) in
    let end_ = clamp_int ~min:start ~max:(Source.length source) end_ in
    Range.create ~source (Byte_index.of_int start) (Byte_index.of_int end_)
  in
  match included_from with
  | None -> []
  | Some loc ->
      let label =
        Diagnostic.Label.secondaryf ~range:(range_of_loc loc)
          "file '%s' included here" filename in
      label :: included_diagnostic ?printed_filename ?code loc

let range_of_loc_span ?printed_filename ?code
    ({begin_loc; end_loc} : Middle.Location_span.t) :
    Range.t * Diagnostic.Label.t list =
  let source = get_source ?printed_filename ?code begin_loc in
  let max = Source.length source in
  let start =
    (* clamp handles errors at end of source *)
    clamp_int ~min:0 ~max begin_loc.byte_num in
  let end_ =
    (* can be needed if error crosses include boundary *)
    clamp_int ~min:start ~max end_loc.byte_num in
  ( Range.create ~source (Byte_index.of_int start) (Byte_index.of_int end_)
  , included_diagnostic ?printed_filename ?code begin_loc )

let unstyle (l : Diagnostic.Label.t) =
  { l with
    message= (fun ppf -> (Fmt.styled `None Diagnostic.Message.pp) ppf l.message)
  }

open Grace_ansi_renderer

let config =
  Config.
    { default with
      styles=
        { Style_sheet.default with
          header_warning= [`Bold; `Fg `Magenta]
        ; primary_label_warning= [`Fg `Magenta]
        ; source_border= [`None; `Faint]
        ; line_number= [`Fg `Yellow] }
    ; chars= {Chars.unicode with source_border_left_break= "⋯"}
    ; num_contextual_lines= 1
    ; enable_inline_contextual_lines= true }

let config ppf =
  let use_ansi =
    match Fmt.style_renderer ppf with `Ansi_tty -> Some true | _ -> Some false
  in
  {config with use_ansi}

let pp ppf d = pp_diagnostic ~config:(config ppf) ?code_to_string:None ppf d

let pp_compact ppf =
  pp_compact_diagnostic ~config:(config ppf) ?code_to_string:None ppf
