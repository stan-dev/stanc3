(** Some helpers to produce nice error messages *)

open Ast
open Core_kernel

type signaturestype = returntype * returntype list [@@deriving sexp, compare]

let string_of_expressiontype = function
  | _, ut -> Sexp.to_string (sexp_of_unsizedtype ut)

let string_of_returntype = function
  | rt -> Sexp.to_string (sexp_of_returntype rt)

let string_of_opt_expressiontype = function
  | None -> "unknown"
  | Some x -> string_of_expressiontype x

(* TODO: implement more pretty printing functions for generating error messages *)
