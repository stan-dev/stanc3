open Core_kernel
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle
open Js_of_ocaml

let stan2cpp model_name model_string =
  Semantic_check.model_name := model_name ;
  let ast =
    Parse.parse_string Parser.Incremental.program model_string
    |> Result.map_error ~f:(Fmt.to_to_string Errors.pp_syntax_error)
  in
  let semantic_err_to_string = function
    | Result.Error (error :: _) ->
        let loc = Semantic_error.location error
        and msg = (Fmt.to_to_string Semantic_error.pp) error in
        Result.Error (Fmt.strf "%a" Errors.pp_semantic_error (msg, loc))
    | Result.Ok _ as ok -> ok
    | Result.Error [] ->
        Result.Error
          "Semantic check failed but reported no errors. This should never \
           happen."
  in
  Result.bind ast
    ~f:
      (Fn.compose semantic_err_to_string Semantic_check.semantic_check_program)
  |> Result.map ~f:(fun typed_ast ->
         let mir = Ast_to_Mir.trans_prog model_name typed_ast in
         let tx_mir = Transform_Mir.trans_prog mir in
         let cpp = Fmt.strf "%a" Stan_math_code_gen.pp_prog tx_mir in
         cpp )

let wrap_result = function
  | Result.Ok s ->
      Js.Unsafe.obj
        [| ("result", Js.Unsafe.inject (Js.string s))
         ; ("warnings", Js.Unsafe.inject Js.array_empty) |]
  | Error e ->
      Js.Unsafe.obj
        [| ("errors", Js.Unsafe.inject (Array.map ~f:Js.string [|e|]))
         ; ("warnings", Js.Unsafe.inject Js.array_empty) |]

let map2 f (x, y) = (f x, f y)

let wrap2 f s1 s2 =
  let s1, s2 = map2 Js.to_string (s1, s2) in
  f s1 s2 |> wrap_result

let () = Js.export "stanc" (wrap2 stan2cpp)
