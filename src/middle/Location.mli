(** Storing locations in the original source *)

(** Source code locations *)
type t = {filename: string; line_num: int; col_num: int; included_from: t option}
[@@deriving sexp, hash]

val compare : t -> t -> int
val empty : t

val pp :
     ?printed_filename:string
  -> ?print_file:bool
  -> ?print_line:bool
  -> unit
  -> t Fmt.t

val pp_context : ((unit -> string list) * t) Fmt.t
