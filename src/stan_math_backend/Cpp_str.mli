(** Utilities for converting OCaml strings into C++ strings *)

val escaped : string -> string
(**
  A version of stdlib {!Bytes.escaped} but
  uses octal output, rather than default decimal,
  for escapes like [\123].
  This allows cpp to read them as literals
*)
