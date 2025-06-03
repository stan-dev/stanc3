(** Wrappers around the APIs provided by [Menhir] *)

val parse_program :
     [< `Code of string | `File of string]
  -> (Ast.untyped_program, Errors.t) result * Warnings.t list
(** Parse a full Stan program *)

val parse_stanfunctions :
     [< `Code of string | `File of string]
  -> (Ast.untyped_program, Errors.t) result * Warnings.t list
(** Parse a "functions-only" [.stanfunctions] file, returning
    an AST (which will only ever have a functions block) or error,
    and warnings *)
