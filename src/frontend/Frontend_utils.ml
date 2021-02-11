open Core_kernel
module Warnings = Middle.Warnings
module Errors = Middle.Errors
module Semantic_error = Middle.Semantic_error

let untyped_ast_of_string s =
  let res, warnings = Parse.parse_string Parser.Incremental.program s in
  Fmt.epr "%a" (Fmt.list ~sep:Fmt.nop Warnings.pp) warnings ;
  res

let typed_ast_of_string_exn s =
  Result.(
    untyped_ast_of_string s
    >>= fun ast ->
    Semantic_check.semantic_check_program ast
    |> map_error ~f:(function
         | err :: _ -> Errors.Semantic_error err
         | _ ->
             failwith
               "Internal compiler error: no message from Semantic_check." ))
  |> Result.map_error ~f:Errors.to_string
  |> Result.ok_or_failwith

let get_ast_or_exit ?printed_filename ?(print_warnings = true) filename =
  let res, warnings = Parse.parse_file Parser.Incremental.program filename in
  if print_warnings then
    Fmt.epr "%a"
      (Fmt.list ~sep:Fmt.nop (Warnings.pp ?printed_filename))
      warnings ;
  match res with
  | Result.Ok ast -> ast
  | Result.Error err -> Errors.pp Fmt.stderr err ; exit 1

let type_ast_or_exit ast =
  try
    match Semantic_check.semantic_check_program ast with
    | Result.Ok prog -> prog
    | Result.Error (error :: _) ->
        Errors.pp_semantic_error Fmt.stderr error ;
        exit 1
    | Result.Error [] ->
        Printf.eprintf
          "Semantic check failed but reported no errors. This should never \
           happen." ;
        exit 1
  with Errors.SemanticError err ->
    Errors.pp_semantic_error Fmt.stderr err ;
    exit 1
