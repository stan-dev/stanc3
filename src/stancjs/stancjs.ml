open Core_kernel
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle
open Js_of_ocaml

let version = "stanc3.js v2.23-rc1"


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

let stan2cpp model_name model_string flags_string =
  let flags = String.split_on_chars ~on:[' '] flags_string in
  let is_flag_set flag  =
   List.mem ~equal:String.equal flags flag in  
  Semantic_check.check_that_all_functions_have_definition :=
    not (is_flag_set "--allow_undefined" || is_flag_set "--allow-undefined") ;
  Stan_math_code_gen.standalone_functions := is_flag_set "--standalone-functions";
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
  (if is_flag_set "--version" then
   Result.Ok (Fmt.strf "%s" version)
  else
    Result.bind ast
      ~f:
        (Fn.compose semantic_err_to_string Semantic_check.semantic_check_program)
    |> Result.map ~f:(fun typed_ast ->
          let mir = Ast_to_Mir.trans_prog model_name typed_ast in
          let uninitialized_vars =
            Dependence_analysis.mir_uninitialized_variables mir
          in
          if is_flag_set "--warn-uninitialized" then print_warn_uninitialized uninitialized_vars ;          
          let tx_mir = Transform_Mir.trans_prog mir in
          let cpp = Fmt.strf "%a" Stan_math_code_gen.pp_prog tx_mir in
          cpp ))

let wrap_result = function
  | Result.Ok s ->
      Js.Unsafe.obj
        [| ("result", Js.Unsafe.inject (Js.string s))
         ; ("warnings", Js.Unsafe.inject (Js.string "")) |]
  | Error e ->
      Js.Unsafe.obj
        [| ("errors", Js.Unsafe.inject (Array.map ~f:Js.string [|e|]))
         ; ("warnings", Js.Unsafe.inject (Js.string "")) |]

let map3 f (x, y, z) = (f x, f y, f z)

let wrap3 f s1 s2 s3 =
  let s1, s2, s3 = map3 Js.to_string (s1, s2, s3) in
  f s1 s2 s3 |> wrap_result

let _ = Js.export "stanc" (wrap3 stan2cpp)
