(* Let's do a simple CSE pass,
ideally expressed as a visitor with a separate visit() function? *)
open Ast
open Core_kernel

let _counter = ref 0;;
let gensym =
  fun () ->
    _counter := (!_counter) + 1;
    String.concat ["sym"; (string_of_int !_counter)];;

(* XXX Add type annotation - returns a list of new symbols *)
let rec count_subtrees_rec fnapp2sym e =
  match e with
  | FnApp(_, args) ->
    let _ = List.map args ~f:(count_subtrees_rec fnapp2sym) in
    Hashtbl.Poly.update fnapp2sym e ~f:(function Some(i) -> i+1 | None -> 1)
  | _ -> ()

let count_subtrees e = let fnapp2sym = Hashtbl.Poly.create () in
  count_subtrees_rec fnapp2sym e;
  fnapp2sym

(* XXX Pull out symbol assignment somewhere else.
   find_dups should just populate the dictionary and filter out
   entries that only occur once *)

let%expect_test "count_subtrees" =
  let dict = count_subtrees (FnApp("plus",
                                   [FnApp("plus", [IntLit 2; IntLit 3]);
                                    FnApp("plus", [IntLit 2; IntLit 3])])) in
  [%sexp (Hashtbl.Poly.to_alist dict : (expr * int) list)]
  |> Sexp.to_string_hum |> print_endline;
  [%expect{|
    ((((FnApp plus ((IntLit 2) (IntLit 3))) (Var sym1))
      ((FnApp plus
        ((FnApp plus ((IntLit 2) (IntLit 3)))
         (FnApp plus ((IntLit 2) (IntLit 3)))))
       (Var sym2)))
     ((Var sym1))) |}]

let filter_dups fnapp2sym =
  let dups = Hashtbl.Poly.filter fnapp2sym ~f:(fun x -> x > 1) in
  Hashtbl.Poly.map dups ~f:(fun _ -> gensym ())

exception NeverHappens

let add_assigns dups e =
  ExprList(
    (Hashtbl.Poly.fold dups ~init:[e] ~f:(fun ~key:ast_node ~data:var l ->
         AssignExpr(var, ast_node) :: l)))

let%expect_test "add_assigns" =
  let ast_node = (FnApp("plus", [IntLit 2])) in
  let dups = Hashtbl.Poly.of_alist_exn [(Var "hi"), "lo"] in
  [%sexp ((add_assigns dups ast_node) : expr)]
|> Sexp.to_string_hum |> print_endline;
  [%expect{| |}]

(* Need to refactor so that we:
   1. go through the AST and find duplicate subtrees (or do GVN?)
   2. then replace duplicates with Var reference to the symbol
   3. then add assignments to the appropriate level (at first, top)
   Also fix find_dups to just find them!

*)

let replaceUsages dups e = match Hashtbl.Poly.find dups e with
  | Some(sym) -> Var sym
  | None -> e

let cse e = let dups = filter_dups (count_subtrees e) in
  replaceUsages dups e

let optimize e =
  e


let%expect_test _ =
[%sexp
      ((optimize (FnApp("plus",
                 [FnApp("plus", [IntLit 2; IntLit 3]);
                  FnApp("plus", [IntLit 2; IntLit 3])])))
    : expr)] |> Sexp.to_string_hum |> print_endline;
  [%expect{|
  (ExprList
   ((FnApp plus
     ((FnApp plus ((IntLit 2) (IntLit 3)))
      (FnApp plus ((IntLit 2) (IntLit 3)))))
    (AssignExpr sym3 (FnApp plus ((IntLit 2) (IntLit 3))))))
|}]
