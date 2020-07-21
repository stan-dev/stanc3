open Core_kernel

let typed_ast_of_string_exn s =
  Parse.parse_string Parser.Incremental.program s
  |> Result.map_error ~f:(Fmt.to_to_string Errors.pp_syntax_error)
  |> Result.ok_or_failwith |> Semantic_check.semantic_check_program
  |> Result.map_error
       ~f:(Fmt.to_to_string (Fmt.list ~sep:Fmt.cut Semantic_error.pp))
  |> Result.ok_or_failwith

let get_ast_or_exit filename =
  try
    match Parse.parse_file Parser.Incremental.program filename with
    | Result.Ok ast -> ast
    | Result.Error err ->
        Errors.pp_syntax_error Fmt.stderr err ;
        exit 1
  with Errors.SyntaxError err ->
    Errors.pp_syntax_error Fmt.stderr err ;
    exit 1

let type_ast_or_exit ast =
  try
    match Semantic_check.semantic_check_program ast with
    | Result.Ok prog -> prog
    | Result.Error (error :: _) ->
        let loc = Semantic_error.location error
        and msg = Fmt.strf "%a" Semantic_error.pp error in
        Errors.pp_semantic_error Fmt.stderr (msg, loc) ;
        exit 1
    | Result.Error [] ->
        Printf.eprintf
          "Semantic check failed but reported no errors. This should never \
           happen." ;
        exit 1
  with Errors.SemanticError err ->
    Errors.pp_semantic_error Fmt.stderr err ;
    exit 1
