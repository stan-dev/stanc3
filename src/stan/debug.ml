(* some helpers for debugging *)

open Syntax
open Core_kernel

(* For s-expression support *)

(* TODO - let these flags come from command line arguments *)

(* Controls whether the parsing operations get logged *)
let grammar_logging = false

(* Controls whether an AST gets printed during parsing *)
let ast_printing = false

(* Controls whether a decorated AST gets printed after the semantic check *)
let decorated_ast_printing = false

let grammar_logger s = if grammar_logging then print_endline s

let ast_to_string x =
  [%sexp (x : Syntax.untyped_program)] |> Sexp.to_string_hum

let ast_logger t = if ast_printing then print_endline (ast_to_string t)

let decorated_ast_logger t =
  if decorated_ast_printing then print_endline (ast_to_string t)
