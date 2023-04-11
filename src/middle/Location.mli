(** Storing locations in the original source *)

(** Source code locations *)
type t = {filename: string; line_num: int; col_num: int; included_from: t option}
[@@deriving sexp, hash]

val empty : t
val compare : t -> t -> int

val to_string :
     ?printed_filename:string
  -> ?print_file:bool
  -> ?print_line:bool
  -> t
  -> string
(** Format the location for error messaging.

    If printed_filename is passed, it replaces the highest-level name and
    leaves the filenames of included files intact. *)

val context_to_string : (unit -> string list) -> t -> string option
(** Turn the given location into a string holding the code of that location.
    Code is retrieved by calling context_cb, which may do IO.
    Exceptions in the callback or in the creation of the string
    (possible if the context is incorrectly too short for the given location)
    return [None] *)

val of_position_opt : Lexing.position -> t option
val of_position_exn : Lexing.position -> t
