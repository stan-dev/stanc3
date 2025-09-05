(** Storing locations in the original source *)

(** Source code locations *)
type t = {filename: string; line_num: int; col_num: int; included_from: t option}
[@@deriving sexp, hash]

val compare : t -> t -> int
val empty : t

val pp :
     ?print_file:bool
  -> ?print_line:bool
  -> (* printed_filename *) string option
  -> t Fmt.t

val pp_context_for : (t * string Array.t) Fmt.t
(* Prints the text surrounding the provided location [t]
   from the given array *)
