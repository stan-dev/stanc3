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

let get_ast_or_exit ?printed_filename ?(print_warnings = true)
    ?(bare_functions = false) filename =
  let res, warnings =
    if bare_functions then
      Parse.parse_file Parser.Incremental.functions_only filename
    else Parse.parse_file Parser.Incremental.program filename
  in
  if print_warnings then
    (Warnings.pp_warnings ?printed_filename) Fmt.stderr warnings ;
  match res with
  | Result.Ok ast -> ast
  | Result.Error err -> Errors.pp Fmt.stderr err ; exit 1

let type_ast_or_exit ast =
  match Typechecker.check_program ast with
  | Result.Ok (p, warns) ->
      Warnings.pp_warnings Fmt.stderr warns ;
      p
  | Result.Error error ->
      Errors.pp_semantic_error Fmt.stderr error ;
      exit 1
