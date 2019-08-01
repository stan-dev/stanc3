open Core_kernel

let typed_ast_of_string_exn s =
  Parse.parse_string Parser.Incremental.program s
  |> Result.map_error ~f:Parse.render_syntax_error
  |> Result.ok_or_failwith |> Semantic_check.semantic_check_program
  |> Result.map_error
       ~f:(Fmt.to_to_string (Fmt.list ~sep:Fmt.cut Semantic_error.pp))
  |> Result.ok_or_failwith

let binop (l : Ast.typed_expression) op (r : Ast.typed_expression) =
  let open Ast in
  let loc = Middle.merge_spans l.emeta.loc r.emeta.loc in
  Semantic_check.semantic_check_binop_exn loc op (l, r)

let loop_bottom emeta =
  {Ast.expr= Ast.IntNumeral "1"; emeta= {emeta with Ast.type_= UInt}}
