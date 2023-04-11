(** Delimited locations in source code *)

type t = {begin_loc: Location.t; end_loc: Location.t}
[@@deriving sexp, hash, compare]

val empty : t
val merge : t -> t -> t

val to_string : ?printed_filename:string -> t -> string
(** Render a location_span as a string *)

val of_positions_opt : Lexing.position -> Lexing.position -> t option
(** Take the Middle.location_span corresponding to a pair of Lexing.position's *)

val of_positions_exn : Lexing.position * Lexing.position -> t
