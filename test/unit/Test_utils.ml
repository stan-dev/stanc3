open Frontend
open Core

let untyped_ast_of_string s =
  let res, warnings = Parse.parse_string Parser.Incremental.program s in
  Fmt.epr "%a" (Fmt.list ~sep:Fmt.nop Warnings.pp) warnings;
  res

let error_to_string ~code =
  Fmt.str "%a" (Errors.pp ?printed_filename:None ?code:(Some code))

let typed_ast_of_string_exn code =
  Result.(
    untyped_ast_of_string code >>= fun ast ->
    Typechecker.check_program ast
    |> map_error ~f:(fun e -> Errors.Semantic_error e))
  |> Result.map_error ~f:(error_to_string ~code)
  |> Result.ok_or_failwith |> fst

let mir_of_string s = typed_ast_of_string_exn s |> Ast_to_Mir.trans_prog ""
