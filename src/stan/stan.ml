(** The main program. *)
(* TODO: this file still contains redundant stuff to do with the interpreter that we have not implemented *)

module CalcVar = Zoo.Main(struct
  let name = "stan"

  type command = Syntax.program

  (** There is no top-level environment as all variables are local *)
  type environment = unit

  (** Should we show compiled code? *)
  let show_code = ref false

  (** RAM size *)
  let ram_size = ref 64

  let options = [
      ("--ram",
       Arg.Set_int ram_size,
       Format.sprintf "<size> RAM size (default: %d)" !ram_size);
      ("--code",
       Arg.Set show_code,
       " Print compiled code")
  ]

  (** At the beginning no variables are defined. *)
  let initial_environment = ()

  let read_more _ = false

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Some (Parser.program Lexer.token)

  (** The command that actually executes a command. It accepts an argument which we can
      ignore, a flag indicating whether we are in ineractive mode, an environment, and a
      command to be excuted. *)
  let exec _ cmd =
    let code = Compile.compile toplevel_parser in
    print_string code;
end) ;;

CalcVar.main ()
