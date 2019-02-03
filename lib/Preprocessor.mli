(** Preprocessor for handling include directives *)

open Core_kernel

val include_stack : Lexing.lexbuf Stack.t
(** Stack with lexing buffers, created from all the includes encountered so
    far *)

val include_paths : string list ref
(** List of paths to search for including files *)

val try_get_new_lexbuf : string -> Lexing.position -> Lexing.lexbuf
(** Search include paths for filename and try to create a new lexing buffer
    with that filename, record that included from specified position *)
