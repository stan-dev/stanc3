open Core_kernel

let typed_ast_of_string_exn s =
  Parse.parse_string Parser.Incremental.program s
  |> Result.map_error ~f:Parse.render_syntax_error
  |> Result.ok_or_failwith |> Semantic_check.semantic_check_program
  |> Result.map ~f:(fun (result, warnings) ->
         ( result
         , Fmt.(to_to_string @@ list ~sep:cut Semantic_warning.pp) warnings )
     )
  |> Result.map_error
       ~f:
         Fmt.(
           to_to_string
           @@ pair ~sep:cut
                (list ~sep:cut Semantic_error.pp)
                (list ~sep:cut Semantic_warning.pp))
  |> Result.ok_or_failwith
