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
  Stan_math_code_gen.standalone_functions := is_flag_set "standalone-functions" ;
  With_return.with_return (fun r ->
      if is_flag_set "version" then
        r.return (Result.Ok (Fmt.strf "%s" version), []) ;
      let ast, warnings =
        try Parse.parse_string Parser.Incremental.program model_string
        with Errors.SyntaxError err -> (Result.Error err, [])
      in
      let warnings = List.map ~f:(Fmt.to_to_string Warnings.pp) warnings in
      let ast =
        Result.map_error ast ~f:(Fmt.to_to_string Errors.pp_syntax_error)
      in
      let open Result.Monad_infix in
      if is_flag_set "auto-format" then
        r.return
          ( (ast >>| fun ast -> Pretty_printing.pretty_print_program ast)
          , warnings ) ;
      let semantic_err_to_string = function
        | error :: _ ->
            let loc = Semantic_error.location error
            and msg = (Fmt.to_to_string Semantic_error.pp) error in
            Fmt.strf "%a" Errors.pp_semantic_error (msg, loc)
        | [] ->
            "Semantic check failed but reported no errors. This should never \
             happen."
      in
      let result =
        ast
        >>= fun ast ->
        let typed_ast =
          Semantic_check.semantic_check_program ast
          |> Result.map_error ~f:semantic_err_to_string
        in
        typed_ast
        >>| fun typed_ast ->
        if is_flag_set "print-canonical" then
          r.return
            ( Result.Ok
                (Pretty_printing.pretty_print_typed_program
                   (Canonicalize.canonicalize_program typed_ast))
            , warnings ) ;
        let mir = Ast_to_Mir.trans_prog model_name typed_ast in
        let tx_mir = Transform_Mir.trans_prog mir in
        let opt_mir =
          if is_flag_set "O" then Optimize.optimization_suite tx_mir
          else tx_mir
        in
        let cpp = Fmt.strf "%a" Stan_math_code_gen.pp_prog opt_mir in
        let uninit_warnings =
          if is_flag_set "warn-uninitialized" then
            [Pedantic_analysis.sprint_warn_uninitialized mir]
          else []
        in
        let pedantic_warnings =
          if is_flag_set "warn-pedantic" then
            [Pedantic_analysis.sprint_warn_pedantic mir]
          else []
        in
        (cpp, warnings @ uninit_warnings @ pedantic_warnings)
      in
      match result with
      | Result.Ok (cpp, warnings) -> (Result.Ok cpp, warnings)
      | Result.Error e -> (Result.Error e, warnings) )

let wrap_result ~warnings = function
  | Result.Ok s ->
      Js.Unsafe.obj
        [| ("result", Js.Unsafe.inject (Js.string s))
         ; ( "warnings"
           , Js.Unsafe.inject
               (Js.array (List.to_array (List.map ~f:Js.string warnings))) )
        |]
  | Error e ->
      Js.Unsafe.obj
        [| ("errors", Js.Unsafe.inject (Array.map ~f:Js.string [|e|]))
         ; ("warnings", Js.Unsafe.inject Js.array_empty) |]

let stan2cpp_wrapped name code (flags : Js.string_array Js.t Js.opt) =
  let result, warnings =
    stan2cpp (Js.to_string name) (Js.to_string code)
      Js.(
        Opt.map flags (fun a ->
            str_array a |> Js.to_array |> Array.map ~f:to_string )
        |> Opt.to_option)
  in
  wrap_result result ~warnings

let () = Js.export "stanc" stan2cpp_wrapped
