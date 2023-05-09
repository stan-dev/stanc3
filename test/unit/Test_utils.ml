open Frontend
open Core_kernel

module CppLibrary : Std_library_utils.Library =
  Stan_math_backend.Stan_math_library

module Typechecker = Typechecking.Make (CppLibrary)
module Ast2Mir = Ast_to_Mir.Make (CppLibrary)

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

let mir_of_string s = typed_ast_of_string_exn s |> Ast2Mir.trans_prog ""

let print_data_prog ast =
  Analysis_and_optimization.Debug_data_generation.gen_values_json
    (Ast2Mir.gather_declarations ast.Ast.datablock)
  |> Result.ok |> Option.value_exn
