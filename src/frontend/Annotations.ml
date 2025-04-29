open Ast
open Core
open Middle

let unknown a = "Unknown annotation '" ^ a ^ "' will be ignored by the compiler"

let unsupported a t =
  "Unsupported annotation '" ^ a ^ "' for type " ^ t
  ^ ". This will be ignored by the compiler"

let unused_list pred id ty anns =
  List.filter_map
    ~f:(fun a ->
      match pred a ty with
      | `Fine -> None
      | `Unknown -> Some (id.id_loc, unknown a)
      | `WrongType ->
          Some (id.id_loc, unsupported a (Fmt.to_to_string UnsizedType.pp ty)))
    anns

let rec collect_stmt pred (acc : Warnings.t list) {stmt; _} =
  match stmt with
  | FunDef {annotations; funname; returntype; arguments; _} ->
      let args = List.map ~f:(fun (ad, ty, _) -> (ad, ty)) arguments in
      let ty =
        UnsizedType.UFun
          ( args
          , returntype
          , Fun_kind.suffix_from_name funname.name
          , Mem_pattern.AoS ) in
      acc @ (unused_list pred funname ty) annotations
  | VarDecl {annotations; variables; decl_type; _} ->
      acc
      @ (unused_list pred (List.hd_exn variables).identifier
           (SizedType.to_unsized decl_type))
          annotations
  | _ -> fold_statement Fn.const (collect_stmt pred) Fn.const Fn.const acc stmt

let find_unrecognized pred (prog : Ast.untyped_program) : Warnings.t list =
  fold_program (collect_stmt pred) [] prog
