open Frontend
open Std
open Result.Syntax

let untyped_ast_of_string s =
  let res, warnings = Parse.parse_program (`Code s) in
  Fmt.epr "%a" (Fmt.list ~sep:Fmt.nop Warnings.pp) warnings;
  res

let error_to_string ~code =
  Fmt.str "%a" (Errors.pp ?printed_filename:None ?code:(Some code))

let typed_ast_of_string_exn code =
  let ast =
    let* ast = untyped_ast_of_string code in
    Typechecker.check_program ~allow_undefined_functions:true ast
    |> Result.map_error ~f:(fun e -> Errors.Semantic_error e) in
  ast |> Result.map_error ~f:(error_to_string ~code) |> Result.get_ok' |> fst

let mir_of_string s = typed_ast_of_string_exn s |> Ast_to_Mir.trans_prog ""
