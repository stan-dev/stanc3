(** The definition of a programming language *)
module type LANGUAGE = sig
  val name : string
  (** The name of the language (used for prompt) *)

  (** The type of top-level commands *)
  type command

  (** The runtime environment *)
  type environment

  val options : (Arg.key * Arg.spec * Arg.doc) list
  (** Additional command-line options *)

  val initial_environment : environment
  (** The initial runtime environment *)

  val file_parser : (Lexing.lexbuf -> command list) option
  (** A parser for parsing entire files *)

  val toplevel_parser : (Lexing.lexbuf -> command) option
  (** A parser for parsing one toplevel command *)

  val exec : environment -> command -> environment
  (** Execute a toplevel command in the given environment and
        return the new environment. *)
end

(** Create a language from its definition. *)
module Main (L : LANGUAGE) : sig
  val main : unit -> unit
  (** The main program *)
end
