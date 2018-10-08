open Core_kernel
open Ast

let function_lookup = Map.of_alist_exn (module String)
    ["add", (+.); "sub", (-.)]

let rec interpret_expr = function
  | IntLit i -> float_of_int i
  | NumLit f -> f
  | FnApp (fname, args) ->
    let f = Map.find_exn function_lookup fname in
    let args = List.map ~f:interpret_expr args in
    f (List.nth_exn args 0) (List.nth_exn args 1)
  | _ -> 0.0

let interpret = function
  | None -> 0.0
  | Prog e -> interpret_expr e
