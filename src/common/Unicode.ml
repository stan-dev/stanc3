let pp_uchar ppf u =
  let u_int = Uchar.to_int u in
  if u_int < 128 then Fmt.string ppf (Char.chr u_int |> Char.escaped)
  else Fmt.pf ppf "U+%04X" u_int

let is_ascii s =
  let rec loop max b i =
    if i > max then true
    else if Bytes.get_uint8 b i < 128 then loop max b (i + 1)
    else false in
  let b = Bytes.of_string s in
  loop (Bytes.length b - 1) b 0

let normalize = Uunf_string.normalize_utf_8 `NFKC

let iter_uchars s f =
  let len = String.length s in
  let out = Buffer.create len in
  let pos = ref 0 in
  (* move through code point by code point *)
  while !pos != len do
    let decode = String.get_utf_8_uchar s !pos in
    let char_length = Uchar.utf_decode_length decode in
    let uchar = Uchar.utf_decode_uchar decode in
    Buffer.add_utf_8_uchar out uchar;
    f !pos uchar;
    pos := !pos + char_length
  done;
  (* another sanity check *)
  let s_after = Buffer.contents out in
  if not (String.equal s s_after) then
    Core.(
      ICE.internal_compiler_error
        [%message
          "Failed to round-trip unicode string!" (s : string) (s_after : string)])
