(* Let's do a simple CSE pass,
ideally expressed as a visitor with a separate visit() function? *)
open Ast
open Core_kernel

type keyType = (string * (expr list))
[@@deriving hash]

let fnapp2sym = Hashtbl.create (module keyType)
(*
let cse = function
  | FnApp (fname, args) ->
*)
