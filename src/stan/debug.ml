(* some helpers for debugging *)

open Syntax
(* open Core_kernel *)  (* For s-expression support *)

let grammar_logging = true (* TODO - let these flags come from command line arguments *)
let ast_printing = false


let grammar_logger s = if grammar_logging then print_string s


let ast_to_string x = "test" (* [%sexp (exp : Syntax.program)] |> Sexp.to_string_hum *)

let asts_to_string t = match t with x::xs -> ast_to_string xs
                                  | _ -> ""

let asts_string t = if ast_printing then ast_to_string t else "Done\n"

