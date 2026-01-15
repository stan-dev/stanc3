(** Some plumbing for our compiler errors *)

type t =
  | FileNotFound of string
  | Syntax_error of Syntax_error.t
  | Semantic_error of Semantic_error.t
  | DebugDataError of (Middle.Location_span.t * string)

val pp : ?printed_filename:string -> ?code:string -> t Fmt.t
(** Pretty-printer for error type [t]. Replace occurrences of filename from
    locations with [printed_filename], if supplied. If [code] is supplied, read
    context from that string. Otherwise, it will attempt to open the original
    file. *)
