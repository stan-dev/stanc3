open Ast
open Std

let get_warnings pred ty anns =
  List.filter_map
    ~f:(fun (a : Middle.Annotation.t) ->
      match pred a.name a.args ty with
      | `Fine -> None
      | `Unknown -> Some (Warnings.unknown_annotation (a.loc, a.name))
      | `WrongType -> Some (Warnings.unsupported_annotation (a.loc, a.name, ty)))
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
      acc @ (get_warnings pred ty) annotations
  | VarDecl {annotations; decl_type; _} ->
      acc
      @ (get_warnings pred (Middle.SizedType.to_unsized decl_type)) annotations
  | _ -> fold_statement Fun.const (collect_stmt pred) Fun.const acc stmt

type annotation_classifier =
     string
  -> string list
  -> Middle.UnsizedType.t
  -> [`Fine | `Unknown | `WrongType]

let find_unrecognized pred (prog : Ast.untyped_program) : Warnings.t list =
  fold_program (collect_stmt pred) [] prog
