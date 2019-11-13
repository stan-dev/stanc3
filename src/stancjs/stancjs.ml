open Core_kernel
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle
open Js_of_ocaml

let print_warn_uninitialized
    (uninit_vars : (Location_span.t * string) Set.Poly.t) =
  let show_var_info (span, var_name) =
    Location_span.to_string span
    ^ ":\n" ^ "  Warning: The variable '" ^ var_name
    ^ "' may not have been initialized.\n"
  in
  let filtered_uninit_vars =
    Set.filter ~f:(fun (span, _) -> span <> Location_span.empty) uninit_vars
  in
  Set.iter filtered_uninit_vars ~f:(fun v_info ->
      Out_channel.output_string stderr (show_var_info v_info) )

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
    | _ -> Result.Error "The impossible happened."
  in
  Result.bind ast
    ~f:
      (Fn.compose semantic_err_to_string Semantic_check.semantic_check_program)
  |> Result.map ~f:(fun typed_ast ->
         let mir = Ast_to_Mir.trans_prog model_name typed_ast in
         let uninitialized_vars =
           Dependence_analysis.mir_uninitialized_variables mir
         in
         print_warn_uninitialized uninitialized_vars ;
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

let _ = Js.export "stanc" (wrap2 stan2cpp)
