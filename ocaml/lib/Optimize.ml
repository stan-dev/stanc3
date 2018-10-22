(* Let's do a simple CSE pass,
ideally expressed as a visitor with a separate visit() function? *)
open Ast
open Core_kernel

let fnapp2sym = Hashtbl.Poly.create ();;
let newSyms = ref []
let _counter = ref 0;;
let gensym =
  fun () ->
    _counter := (!_counter) + 1;
    String.concat ["sym"; (string_of_int !_counter)];;

let mk_var () = Var (gensym ())

let mk_assign e =
  fun () ->
    AssignExpr ((gensym ()), e)

let rec find_dups e = match e with
  | FnApp(fname, args) -> (
      let args = List.map args ~f:find_dups in
      let new_fn_app = FnApp (fname, args) in
      match Hashtbl.Poly.find fnapp2sym new_fn_app with
      | None -> let newVar = mk_var () in
        Hashtbl.Poly.add_exn fnapp2sym ~key:new_fn_app ~data:newVar;
        newVar
      | Some(sym) -> newSyms := sym :: !newSyms; sym
      (*Hashtbl.Poly.find_or_add fnapp2sym new_fn_app ~default:mk_var*)
    )
  | _ -> e

let invertTable m =
  Hashtbl.Poly.fold m ~init:(Hashtbl.Poly.create ())
    ~f:(fun ~key:k ~data:v map ->
        Hashtbl.Poly.add_exn map ~key:v ~data:k;
        map)

let%expect_test "invertTable" =
  [%sexp
    ([0, "zero"; 1, "one"; 2, "two"]
     |>    Hashtbl.Poly.of_alist_exn
     |> invertTable
     : ((string, int) Hashtbl.Poly.t))]
  |> Sexp.to_string_hum |> print_endline;
  [%expect{| ((one 1) (two 2) (zero 0)) |}]


exception NeverHappens

let addAssigns e =
  let defTable = invertTable fnapp2sym in
  let newSyms' = !newSyms in
  let defs = List.map newSyms' ~f:(Hashtbl.find_exn defTable) in
  let assigns =
    List.map (List.zip_exn newSyms' defs)
      ~f:(function
            (Var(s), d) -> AssignExpr(s, d)
          | _ -> raise NeverHappens
         )
  in
  ExprList(List.append [e] assigns)

(* Need to refactor so that we:
   1. go through the AST and find duplicate subtrees (or do GVN?)
   2. then replace duplicates with Var reference to the symbol
   3. then add assignments to the appropriate level (at first, top)
   Also fix find_dups to just find them!

let replaceUsages e = 1;;

*)

let optimize e =
  let newExpr = find_dups e in
  let e = addAssigns newExpr in
  e


let%expect_test _ =
[%sexp
      ((optimize (FnApp("plus",
                 [FnApp("plus", [IntLit 2; IntLit 3]);
                  FnApp("plus", [IntLit 2; IntLit 3])])))
    : expr)] |> Sexp.to_string_hum |> print_endline;
  [%expect{|
  (ExprList
   ((Var sym2) (AssignExpr sym1 (FnApp plus ((IntLit 2) (IntLit 3))))))
|}]
