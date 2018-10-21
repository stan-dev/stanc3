(* Let's do a simple CSE pass,
ideally expressed as a visitor with a separate visit() function? *)
open Ast
open Core_kernel
(*
type keyType = (string * (expr list))
[@@deriving hash]

let fnapp2sym = Hashtbl.create (module keyType)
*)
let fnapp2sym = Hashtbl.Poly.create ();;
let _counter = ref 0;;
let gensym =
  fun () ->
    _counter := (!_counter) + 1;
    String.concat ["sym"; (string_of_int !_counter)];;

let mk_assign e =
  fun () ->
    AssignExpr ((gensym ()), e)

let rec cse e = match e with
  | FnApp(fname, args) ->
    let args = List.map args ~f:cse in
    let new_fn_app = FnApp (fname, args) in
    Hashtbl.Poly.find_or_add fnapp2sym new_fn_app ~default:(mk_assign new_fn_app)
  | _ -> e


let%expect_test _ =
print_endline "Hello, world!";
[%expect{|
  Hello, world!
|}]
