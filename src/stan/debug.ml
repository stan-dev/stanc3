(* some helpers for debugging *)

open Syntax

let grammar_logging = true (* TODO - let these flags come from command line arguments *)
let ast_printing = false


let grammar_logger s = if grammar_logging then print_string s


let ast_to_string x = "test\n" (* Sexp.to_string (sexp_of_program x) *)

let asts_to_string t = match t with x::xs -> ast_to_string xs
                                  | _ -> ""

let asts_string t = if ast_printing then ast_to_string t else "Done\n"

