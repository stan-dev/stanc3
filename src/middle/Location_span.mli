(** Delimited locations in source code *)

type t = {begin_loc: Location.t; end_loc: Location.t}
[@@deriving sexp, hash, compare]

val empty : t
val merge : t -> t -> t
val pp : ?printed_filename:string -> t Fmt.t

val to_string : ?printed_filename:string -> t -> string
(** Render a location_span as a string *)
