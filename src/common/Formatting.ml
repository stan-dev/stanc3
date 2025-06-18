open Core
open Core.Poly

let strip_ansi_escapes s =
  (* based on code in https://github.com/b0-system/b0/blob/5695ea679915d54bdf5910e798897a5c3e03bbac/src/std/b0__fmt.ml#L612-L634 *)
  let buf = Buffer.create (String.length s) in
  let out_string s first len =
    Buffer.add_string buf (String.sub s ~pos:first ~len) in
  let max = String.length s - 1 in
  let flush first last =
    if first > last then () else out_string s first (last - first + 1) in
  let rec skip_esc i =
    if i > max then scan i i
    else
      let k = i + 1 in
      if s.[i] = 'm' then scan k k else skip_esc k
  and scan first i =
    if i > max then flush first max
    else
      match s.[i] with
      | '\x1B' ->
          flush first (i - 1);
          skip_esc (i + 1)
      | _ -> scan first (i + 1) in
  scan 0 0;
  Buffer.contents buf
