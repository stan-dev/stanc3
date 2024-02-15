open Common.Unicode

let allow_unicode = ref false

let error ~loc msg =
  raise
    (Errors.SyntaxError
       (Errors.Lexing (msg, Preprocessor.location_of_position loc)))

let validate_ascii_id ~loc id =
  Debugging.lexer_logger ("ascii id: " ^ id);
  let first = String.get_uint8 id 0 in
  if
    (first >= Char.code 'A' && first <= Char.code 'Z')
    || (first >= Char.code 'a' && first <= Char.code 'z')
  then id
  else error ~loc "Invalid character found."

(* Validation based on the
   Unicode Standard Annex #31: Unicode Identifiers and Syntax
   https://www.unicode.org/reports/tr31 *)

let validate_utf8_id ~loc id =
  if not !allow_unicode then
    error ~loc
      "Unicode identifiers are not supported without the (experimental) \
       allow-unicode flag";
  if not (String.is_valid_utf_8 id) then
    error ~loc "Identifier is not valid UTF-8 string";
  Debugging.lexer_logger ("unicode id: " ^ id);
  (* normalize to NFKC as recommended *)
  let id = normalize id in
  let f pos uchar =
    if pos == 0 then (
      if not (Uucp.Id.is_xid_start uchar) then
        error ~loc (Fmt.str "Invalid character: '%a'" pp_uchar uchar))
    else if not (Uucp.Id.is_xid_continue uchar) then
      error ~loc
        (Fmt.str "Invalid character in identifier at offset %d: '%a'" pos
           pp_uchar uchar) in
  iter_uchars id f;
  id

let validate loc id =
  if is_ascii id then validate_ascii_id ~loc id else validate_utf8_id ~loc id
