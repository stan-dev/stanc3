let error ~loc msg =
  raise
    (Errors.SyntaxError
       (Errors.Lexing (msg, Preprocessor.location_of_position loc)))

let pp_uchar ppf u =
  let u_int = Uchar.to_int u in
  if u_int < 128 then Fmt.string ppf (Char.chr u_int |> Char.escaped)
  else Fmt.pf ppf "U+%04X" u_int

(* Validation based on the
   Unicode Standard Annex #31: Unicode Identifiers and Syntax
   https://www.unicode.org/reports/tr31 *)

let validate_identifier loc id =
  (* sanity check *)
  if not (String.is_valid_utf_8 id) then
    error "Identifier is not valid UTF-8 string" ~loc;
  (* normalize to NFKC as recommended *)
  let id = Uunf_string.normalize_utf_8 `NFKC id in
  let out = Buffer.create 24 in
  let len = String.length id in
  let pos = ref 0 in
  (* move through code point by code point *)
  while !pos != len do
    let decode = String.get_utf_8_uchar id !pos in
    let char_length = Uchar.utf_decode_length decode in
    let uchar = Uchar.utf_decode_uchar decode in
    Buffer.add_utf_8_uchar out uchar;
    match !pos with
    | 0 when not (Uucp.Id.is_xid_start uchar) ->
        error ~loc (Fmt.str "Invalid character: '%a'" pp_uchar uchar)
    | _ when not (Uucp.Id.is_xid_continue uchar) ->
        error ~loc
          (Fmt.str "Invalid character in identifier at offset %d: '%a'" !pos
             pp_uchar uchar)
    | _ -> pos := !pos + char_length
  done;
  (* another sanity check *)
  let res_id = Buffer.contents out in
  (if not (String.equal res_id id) then
     Core.(
       Common.FatalError.fatal_error_msg
         [%message "Failed to properly encode id during lexing!" (id : string)]));
  id
