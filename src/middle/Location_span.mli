(** Delimited locations *)
type t = {begin_loc: Location.t; end_loc: Location.t}
[@@deriving sexp, hash, compare]

val empty : t
val merge : t -> t -> t
val to_string : t -> string
val of_positions_opt : Lexing.position -> Lexing.position -> t option
