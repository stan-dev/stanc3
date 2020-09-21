open Core_kernel
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle
open Js_of_ocaml

let version = "%%NAME%% %%VERSION%%"

let warn_uninitialized_msgs
    (uninit_vars : (Location_span.t * string) Set.Poly.t) =
  let show_var_info (span, var_name) =
    Location_span.to_string span
    ^ ":\n" ^ "  Warning: The variable '" ^ var_name
    ^ "' may not have been initialized.\n"
  in
  let filtered_uninit_vars =
    Set.filter ~f:(fun (span, _) -> span <> Location_span.empty) uninit_vars
  in
  Set.Poly.(to_list (map filtered_uninit_vars ~f:show_var_info))

let stan2cpp model_name model_string flags =
  let is_flag_set =
    match flags with
    | Some flags -> fun flag -> Array.mem ~equal:String.equal flags flag
    | None -> fun _ -> false
  in
  Semantic_check.model_name := model_name ;
  Semantic_check.check_that_all_functions_have_definition :=
    not (is_flag_set "allow_undefined" || is_flag_set "allow-undefined") ;
  Transform_Mir.use_opencl := is_flag_set "use-opencl" ;
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
  if is_flag_set "version" then Result.Ok (Fmt.strf "%s" version, [])
  else
    Result.bind ast
      ~f:
        (Fn.compose semantic_err_to_string
           Semantic_check.semantic_check_program)
    |> Result.map ~f:(fun typed_ast ->
           if is_flag_set "print-canonical" then
             ( Pretty_printing.pretty_print_typed_program
                 (Canonicalize.canonicalize_program typed_ast)
             , [] )
           else if is_flag_set "auto-format" then
             match ast with
             | Result.Ok f -> (Pretty_printing.pretty_print_program f, [])
             | _ -> ("Error: This should not happend!", [])
           else
             let mir = Ast_to_Mir.trans_prog model_name typed_ast in
             let tx_mir = Transform_Mir.trans_prog mir in
             let opt_mir =
               if is_flag_set "O" then Optimize.optimization_suite tx_mir
               else tx_mir
             in
             let cpp = Fmt.strf "%a" Stan_math_code_gen.pp_prog opt_mir in
             if is_flag_set "warn-uninitialized" then
               Pedantic_analysis.print_warn_uninitialized mir ;
             if is_flag_set "warn-pedantic" then
               Pedantic_analysis.print_warn_pedantic mir ;
             (cpp, []) )

let wrap_result = function
  | Result.Ok (s, k) ->
      Js.Unsafe.obj
        [| ("result", Js.Unsafe.inject (Js.string s))
         ; ( "warnings"
           , Js.Unsafe.inject
               (Js.array (List.to_array (List.map ~f:Js.string k))) ) |]
  | Error e ->
      Js.Unsafe.obj
        [| ("errors", Js.Unsafe.inject (Array.map ~f:Js.string [|e|]))
         ; ("warnings", Js.Unsafe.inject Js.array_empty) |]

let stan2cpp_wrapped name code (flags : Js.string_array Js.t Js.opt) =
  stan2cpp (Js.to_string name) (Js.to_string code)
    Js.(
      Opt.map flags (fun a ->
          str_array a |> Js.to_array |> Array.map ~f:to_string )
      |> Opt.to_option)
  |> wrap_result

let () = Js.export "stanc" stan2cpp_wrapped
