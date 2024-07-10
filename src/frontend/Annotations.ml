open Ast
open Std

let unused_list pred ty anns =
  List.filter_map
    ~f:(fun a ->
      match pred a.name ty with
      | `Fine -> None
      | `Unknown -> Some (Warnings.unknown_annotation (a.id_loc, a.name))
      | `WrongType ->
          Some (Warnings.unsupported_annotation (a.id_loc, a.name, ty)))
    anns

let rec collect_stmt pred (acc : Warnings.t list) {stmt; _} =
  match stmt with
  | FunDef {annotations; funname; returntype; arguments; _} ->
      let args = List.map ~f:(fun (ad, ty, _) -> (ad, ty)) arguments in
      let ty =
        Middle.(
          UnsizedType.UFun
            ( args
            , returntype
            , Fun_kind.suffix_from_name funname.name
            , Mem_pattern.AoS )) in
      acc @ (unused_list pred ty) annotations
  | VarDecl {annotations; decl_type; _} ->
      acc
      @ (unused_list pred (Middle.SizedType.to_unsized decl_type)) annotations
  | _ -> fold_statement Fun.const (collect_stmt pred) Fun.const acc stmt

type annotation_classifier =
  string -> Middle.UnsizedType.t -> [`Fine | `Unknown | `WrongType]

let find_unrecognized pred (prog : Ast.untyped_program) : Warnings.t list =
  fold_program (collect_stmt pred) [] prog
