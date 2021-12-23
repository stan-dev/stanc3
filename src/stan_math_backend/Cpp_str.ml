open Bytes

let escaped_b s =
  let n = ref 0 in
  for i = 0 to length s - 1 do
    n :=
      !n
      +
      match unsafe_get s i with
      | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
      | ' ' .. '~' -> 1
      | _ -> 4
  done ;
  if !n = length s then copy s
  else
    let s' = create !n in
    n := 0 ;
    for i = 0 to length s - 1 do
      ( match unsafe_get s i with
      | ('\"' | '\\') as c ->
          unsafe_set s' !n '\\' ; incr n ; unsafe_set s' !n c
      | '\n' -> unsafe_set s' !n '\\' ; incr n ; unsafe_set s' !n 'n'
      | '\t' -> unsafe_set s' !n '\\' ; incr n ; unsafe_set s' !n 't'
      | '\r' -> unsafe_set s' !n '\\' ; incr n ; unsafe_set s' !n 'r'
      | '\b' -> unsafe_set s' !n '\\' ; incr n ; unsafe_set s' !n 'b'
      | ' ' .. '~' as c -> unsafe_set s' !n c
      | c ->
          let a = Char.code c in
          unsafe_set s' !n '\\' ;
          incr n ;
          (* changed *)
          unsafe_set s' !n (Char.chr (48 + (a / 64))) ;
          incr n ;
          (* changed *)
          unsafe_set s' !n (Char.chr (48 + (a / 8 mod 8))) ;
          incr n ;
          (* changed *)
          unsafe_set s' !n (Char.chr (48 + (a mod 8))) ) ;
      incr n
    done ;
    s'

open String

let escaped s : string =
  let rec escape_if_needed (s : string) n i =
    if i >= n then s
    else
      match unsafe_get s i with
      | '\"' | '\\' | '\000' .. '\031' | '\127' .. '\255' ->
          Bytes.to_string (escaped_b (Bytes.of_string s))
      | _ -> escape_if_needed s n (i + 1) in
  escape_if_needed s (length s) 0
