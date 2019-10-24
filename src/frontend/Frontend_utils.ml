open Core_kernel

let typed_ast_of_string_exn s =
  Parse.parse_string Parser.Incremental.program s
  |> Result.map_error ~f:Parse.render_syntax_error
  |> Result.ok_or_failwith |> Semantic_check.semantic_check_program
  |> Result.map ~f:(fun (p, ws) ->
         (p, Fmt.(to_to_string (list ~sep:cut Semantic_warning.pp) ws)) )
  |> Result.map_error
       ~f:
         Fmt.(
           to_to_string
           @@ pair ~sep:cut
                (list ~sep:cut Semantic_error.pp)
                (list ~sep:cut Semantic_warning.pp))
  |> Result.ok_or_failwith

let get_ast_or_exit filename =
  try
    match Parse.parse_file Parser.Incremental.program filename with
    | Result.Ok ast -> ast
    | Result.Error err ->
        let loc = Parse.syntax_error_location err
        and msg = Parse.syntax_error_message err in
        Errors.report_parsing_error (msg, loc) ;
        exit 1
  with Errors.SyntaxError err ->
    Errors.report_syntax_error err ;
    exit 1

let type_ast_or_exit ast =
  try
    match Semantic_check.semantic_check_program ast with
    | Result.Ok prog -> prog
    | Result.Error (error :: _, _) ->
        let loc = Semantic_error.location error
        and msg = (Fmt.to_to_string Semantic_error.pp) error in
        Errors.report_semantic_error (msg, loc) ;
        exit 1
    | _ ->
        Printf.eprintf "The impossible happened" ;
        exit 1
  with Errors.SemanticError err ->
    Errors.report_semantic_error err ;
    exit 1
