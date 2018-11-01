(** The main program. *)
open Syntax

module CalcVar = Zoo.Main (struct
  let name = "stan"

  type command = Syntax.program

  (** There is no top-level environment as all variables are local *)
  type environment = unit

  let options = []

  (** At the beginning no variables are defined. *)
  let initial_environment = ()

  let read_more _ = false

  let file_parser = Some (Parser.file Lexer.token)

  let toplevel_parser = Some (Parser.program Lexer.token)

  (** The command that actually executes a command. *)
  let exec _ p =
    let _ = Semantic_check.semantic_check_program p in
    ()
end)

;;
CalcVar.main ()
