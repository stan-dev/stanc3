(** Some helpers for debugging *)

open Core_kernel

(* For s-expression support *)

(** Controls whether the lexing operations get logged *)
let lexer_logging = ref false

let lexer_logger s = if !lexer_logging then print_endline s

(** Controls whether the parsing operations get logged *)
let grammar_logging = ref false

let grammar_logger s = if !grammar_logging then print_endline s

(** Controls whether an AST gets printed during parsing *)
let ast_printing = ref false

let ast_to_string x = [%sexp (x : Ast.untyped_program)] |> Sexp.to_string_hum
let ast_logger t = if !ast_printing then print_endline (ast_to_string t)

(** Controls whether a decorated AST gets printed after the semantic check *)
let typed_ast_printing = ref false

let typed_ast_to_string x =
  [%sexp (x : Ast.typed_program)] |> Sexp.to_string_hum

let typed_ast_logger t =
  if !typed_ast_printing then print_endline (typed_ast_to_string t)
