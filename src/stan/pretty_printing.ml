(** Some helpers to produce nice error messages *)

open Ast

let rec string_of_unsizedtype = function
  | Int -> "int"
  | Real -> "real"
  | Vector -> "vector"
  | RowVector -> "row_vector"
  | Matrix -> "matrix"
  | Array ut -> string_of_unsizedtype ut ^ "[]"
  | Fun (argtypes, rt) ->
      "("
      ^ String.concat ", " (List.map string_of_argtype argtypes)
      ^ ") => " ^ string_of_returntype rt
  | PrimitiveFunction -> "Stan Math function"

and string_of_returntype = function
  | ReturnType x -> string_of_unsizedtype x
  | Void -> "void"

and string_of_argtype = function
  | ob, ut -> string_of_originblock ob ^ string_of_unsizedtype ut

and string_of_originblock = function Data | TData -> "data " | _ -> ""

let string_of_expressiontype = function _, ut -> string_of_unsizedtype ut

let string_of_opt_expressiontype = function
  | None -> "unknown"
  | Some x -> string_of_expressiontype x

(* TODO: implement more pretty printing functions for generating error messages *)
