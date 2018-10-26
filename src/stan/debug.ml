(* some helpers for debugging *)

open Syntax
open Core_kernel  (* For s-expression support *)

(* TODO - let these flags come from command line arguments *)
let grammar_logging = true (* Controls whether the parsing operations get logged *)
let ast_printing = true (* Controls whether an AST gets printed during parsing *)


let grammar_logger s = if grammar_logging then print_endline s


let ast_to_string x = [%sexp (x : Syntax.program)] |> Sexp.to_string_hum

let ast_logger t = if ast_printing then print_endline (ast_to_string t)

