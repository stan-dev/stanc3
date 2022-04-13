open! Core_kernel

let warnings = ref []
let init () = warnings := []
let collect () = List.rev !warnings
let add_warning span message = warnings := (span, message) :: !warnings

let empty file =
  add_warning Middle.Location_span.empty
    ( "Empty file '" ^ file
    ^ "' detected; this is a valid stan model but likely unintended!" )

let deprecated token (pos, message) =
  (* TODO(seantalts): should we only print deprecation warnings once per token? *)
  let begin_pos =
    {pos with Lexing.pos_cnum= pos.Lexing.pos_cnum - String.length token} in
  let end_pos = {begin_pos with Lexing.pos_cnum= pos.pos_cnum - 1} in
  let span =
    Middle.Location_span.of_positions_opt begin_pos end_pos
    |> Option.value ~default:Middle.Location_span.empty in
  add_warning span message

let array_syntax ?(unsized = false) (pos1, pos2) =
  let placement = if unsized then "a type" else "a variable name" in
  add_warning
    (Option.value ~default:Middle.Location_span.empty
       (Middle.Location_span.of_positions_opt pos1 pos2) )
    ( "Declaration of arrays by placing brackets after " ^ placement
    ^ " is deprecated and will be removed in Stan 2.32.0. Instead use the \
       array keyword before the type. This can be changed automatically using \
       the auto-format flag to stanc" )

let drop_array_future () =
  match !warnings with
  | ( _
    , "Variable name 'array' will be a reserved word starting in Stan 2.32.0. \
       Please rename it!" )
    :: tl ->
      warnings := tl
  | _ -> ()

let future_keyword kwrd version (pos1, pos2) =
  add_warning
    (Option.value ~default:Middle.Location_span.empty
       (Middle.Location_span.of_positions_opt pos1 pos2) )
    ( "Variable name '" ^ kwrd ^ "' will be a reserved word starting in Stan "
    ^ version ^ ". Please rename it!" )
