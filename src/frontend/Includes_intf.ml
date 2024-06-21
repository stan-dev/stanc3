(** Functor declarations for preprocessing *)

module type LEXBUF_LOCATOR = sig
  val find_include : string -> Lexing.lexbuf * string
  (** Injectable code for finding an included file. In the
    binary stanc, this will access the filesystem, while in
    stancjs it is a look up in a map object.
  *)
end

module type PREPROCESSOR_LOADER = sig
  val try_get_new_lexbuf : string -> Lexing.lexbuf
  (** Search include paths for filename and try to create a new lexing buffer
        with that filename, record that included from specified position *)
end
