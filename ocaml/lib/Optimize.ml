(* Let's do a simple CSE pass,
ideally expressed as a visitor with a separate visit() function? *)
open Ast
open Core_kernel

let _counter = ref 0;;
let gensym =
  fun () ->
    _counter := (!_counter) + 1;
    String.concat ["sym"; (string_of_int !_counter)];;

let rec count_subtrees_rec fnapp2sym e =
  match e with
  | FnApp(_, args) ->
    let _ = List.map args ~f:(count_subtrees_rec fnapp2sym) in
    Hashtbl.Poly.update fnapp2sym e ~f:(function Some(i) -> i+1 | None -> 1)
  | _ -> ()

let count_subtrees e = let fnapp2sym = Hashtbl.Poly.create () in
  count_subtrees_rec fnapp2sym e;
  fnapp2sym

let%expect_test "count_subtrees" =
  let dict = count_subtrees (FnApp("plus",
                                   [FnApp("plus", [IntLit 2; IntLit 3]);
                                    FnApp("plus", [IntLit 2; IntLit 3])])) in
  print_s [%sexp (Hashtbl.Poly.to_alist dict : (expr * int) list)];
  [%expect{|
    (((FnApp plus ((IntLit 2) (IntLit 3))) 2)
     ((FnApp plus
       ((FnApp plus ((IntLit 2) (IntLit 3)))
        (FnApp plus ((IntLit 2) (IntLit 3)))))
      1)) |}]

let filter_dups fnapp2sym =
  let dups = Hashtbl.Poly.filter fnapp2sym ~f:(fun x -> x > 1) in
  Hashtbl.Poly.map dups ~f:(fun _ -> gensym ())

let add_assigns dups e =
  ExprList(
    (Hashtbl.Poly.fold dups ~init:[e] ~f:(fun ~key:ast_node ~data:var l ->
         AssignExpr(var, ast_node) :: l)))

let%expect_test "add_assigns" =
  let ast_node = (FnApp("plus", [IntLit 2])) in
  let dups = Hashtbl.Poly.of_alist_exn [(Var "hi"), "lo"] in
  print_s [%sexp ((add_assigns dups ast_node) : expr)];
  [%expect{| (ExprList ((AssignExpr lo (Var hi)) (FnApp plus ((IntLit 2))))) |}]

let rec replace_usages dups e = match Hashtbl.Poly.find dups e with
  | Some(sym) -> Var sym
  | None -> (match e with
      | FnApp(fname, args) -> FnApp(fname, List.map args
                                      ~f:(replace_usages dups))
      | x -> x)

let%expect_test "replace_usages" =
  let e = (FnApp("p", [IntLit 2])) in
  let dups = Hashtbl.Poly.of_alist_exn [e, "sup"] in
  print_s [%sexp ((replace_usages dups e) : expr)];
  [%expect{| (Var sup) |}]

let cse e = let dups = filter_dups (count_subtrees e) in
  add_assigns dups (replace_usages dups e)

let optimize e = cse e

let%expect_test _ =
  print_s [%sexp
    ((optimize (FnApp("plus",
                      [FnApp("plus", [IntLit 2; IntLit 3]);
                       FnApp("plus", [IntLit 2; IntLit 3])])))
     : expr)];
  [%expect{|
      (ExprList
       ((AssignExpr sym1 (FnApp plus ((IntLit 2) (IntLit 3))))
        (FnApp plus ((Var sym1) (Var sym1))))) |}]
