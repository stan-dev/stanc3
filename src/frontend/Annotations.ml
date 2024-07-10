open Ast
open Core

let message a = "Unknown annotation '" ^ a ^ "' will be ignored by the compiler"

let unused_list pred id anns =
  List.filter_map
    ~f:(fun a -> if pred a then None else Some (id.id_loc, message a))
    anns

let rec collect_stmt pred (acc : Warnings.t list) {stmt; _} =
  match stmt with
  | FunDef {annotations; funname; _} ->
      acc @ (unused_list pred funname) annotations
  | VarDecl {annotations; variables; _} ->
      acc @ (unused_list pred (List.hd_exn variables).identifier) annotations
  | _ -> fold_statement Fn.const (collect_stmt pred) Fn.const Fn.const acc stmt

let find_unrecognized (pred : string -> bool) (prog : Ast.untyped_program) :
    Warnings.t list =
  fold_program (collect_stmt pred) [] prog
