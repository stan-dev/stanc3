open Frontend
open Core_kernel

let untyped_ast_of_string s =
  let res, warnings = Parse.parse_string Parser.Incremental.program s in
  Fmt.epr "%a" (Fmt.list ~sep:Fmt.nop Warnings.pp) warnings ;
  res

let typed_ast_of_string_exn s =
  Result.(
    untyped_ast_of_string s
    >>= fun ast ->
    Typechecker.check_program ast
    |> map_error ~f:(fun e -> Errors.Semantic_error e))
  |> Result.map_error ~f:Errors.to_string
  |> Result.ok_or_failwith |> fst

let mir_of_string s = typed_ast_of_string_exn s |> Ast_to_Mir.trans_prog ""
