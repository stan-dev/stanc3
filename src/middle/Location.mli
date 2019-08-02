(** Source code locations *)
type t =
  {filename: string; line_num: int; col_num: int; included_from: t option}
[@@deriving sexp, hash, compare]

val pp_with_message : Format.formatter -> string * t -> unit
val empty : t
val to_string : ?print_file:bool -> ?print_line:bool -> t -> string
val of_string_opt : string -> t option
val of_position_opt : Lexing.position -> t option
