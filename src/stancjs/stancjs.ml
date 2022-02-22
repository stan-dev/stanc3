open Core_kernel
open Core_kernel.Poly
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle
open Js_of_ocaml

let version = "%%NAME%% %%VERSION%%"

let warn_uninitialized_msgs (uninit_vars : (Location_span.t * string) Set.Poly.t)
    =
  let show_var_info (span, var_name) =
    Location_span.to_string span
    ^ ":\n" ^ "  Warning: The variable '" ^ var_name
    ^ "' may not have been initialized.\n" in
  let filtered_uninit_vars =
    Set.filter ~f:(fun (span, _) -> span <> Location_span.empty) uninit_vars
  in
  Set.Poly.(to_list (map filtered_uninit_vars ~f:show_var_info))

let stan2cpp model_name model_string is_flag_set flag_val =
  Common.Gensym.reset_danger_use_cautiously () ;
  Typechecker.model_name := model_name ;
  Typechecker.check_that_all_functions_have_definition :=
    not (is_flag_set "allow_undefined" || is_flag_set "allow-undefined") ;
  Transform_Mir.use_opencl := is_flag_set "use-opencl" ;
  Stan_math_code_gen.standalone_functions :=
    is_flag_set "standalone-functions" || is_flag_set "functions-only" ;
  With_return.with_return (fun r ->
      if is_flag_set "version" then
        r.return (Result.Ok (Fmt.str "%s" version), [], []) ;
      let ast, parser_warnings =
        if is_flag_set "functions-only" then
          Parse.parse_string Parser.Incremental.functions_only model_string
        else Parse.parse_string Parser.Incremental.program model_string in
      let open Result.Monad_infix in
      let result =
        ast
        >>= fun ast ->
        let typed_ast =
          Typechecker.check_program ast
          |> Result.map_error ~f:(fun e -> Errors.Semantic_error e) in
        typed_ast
        >>| fun (typed_ast, type_warnings) ->
        let warnings = parser_warnings @ type_warnings in
        if is_flag_set "info" then
          r.return (Result.Ok (Info.info typed_ast), warnings, []) ;
        let canonicalizer_settings =
          if is_flag_set "print-canonical" then Canonicalize.all
          else
            match flag_val "canonicalize" with
            | None -> Canonicalize.none
            | Some s ->
                let parse settings s =
                  match String.lowercase s with
                  | "deprecations" ->
                      Canonicalize.{settings with deprecations= true}
                  | "parentheses" -> {settings with parentheses= true}
                  | "braces" -> {settings with braces= true}
                  (* this probably never applies to stancjs, but for completion: *)
                  | "includes" -> {settings with inline_includes= true}
                  | _ -> settings in
                List.fold ~f:parse ~init:Canonicalize.none
                  (String.split ~on:',' s) in
        let line_length =
          flag_val "max-line-length"
          |> Option.map ~f:int_of_string
          |> Option.value ~default:78 in
        let mir = Ast_to_Mir.trans_prog model_name typed_ast in
        let tx_mir = Transform_Mir.trans_prog mir in
        if is_flag_set "auto-format" || is_flag_set "print-canonical" then
          r.return
            ( Result.Ok
                (Pretty_printing.pretty_print_typed_program
                   ~bare_functions:(is_flag_set "functions-only")
                   ~line_length
                   ~inline_includes:canonicalizer_settings.inline_includes
                   (Canonicalize.canonicalize_program typed_ast
                      canonicalizer_settings ) )
            , warnings
            , [] ) ;
        if is_flag_set "debug-mir" then
          r.return
            ( Result.Ok
                (Sexp.to_string_hum [%sexp (mir : Middle.Program.Typed.t)])
            , warnings
            , [] ) ;
        if is_flag_set "debug-mir-pretty" then
          r.return (Result.Ok (Fmt.str "%a" Program.Typed.pp mir), warnings, []) ;
        if is_flag_set "debug-generate-data" then
          r.return
            ( Result.Ok
                (Debug_data_generation.print_data_prog
                   (Ast_to_Mir.gather_data typed_ast) )
            , warnings
            , [] ) ;
        let opt_mir =
          let opt_lvl =
            if is_flag_set "O0" then Optimize.O0
            else if is_flag_set "O1" then Optimize.O1
            else if is_flag_set "Oexperimental" || is_flag_set "O" then
              Optimize.Oexperimental
            else Optimize.O0 in
          Optimize.optimization_suite
            ~settings:(Optimize.level_optimizations opt_lvl)
            tx_mir in
        if is_flag_set "debug-optimized-mir" then
          r.return
            ( Result.Ok
                (Sexp.to_string_hum [%sexp (opt_mir : Middle.Program.Typed.t)])
            , warnings
            , [] ) ;
        if is_flag_set "debug-optimized-mir-pretty" then
          r.return
            (Result.Ok (Fmt.str "%a" Program.Typed.pp opt_mir), warnings, []) ;
        if is_flag_set "debug-transformed-mir" then
          r.return
            ( Result.Ok
                (Sexp.to_string_hum [%sexp (tx_mir : Middle.Program.Typed.t)])
            , warnings
            , [] ) ;
        if is_flag_set "debug-transformed-mir-pretty" then
          r.return
            (Result.Ok (Fmt.str "%a" Program.Typed.pp tx_mir), warnings, []) ;
        let cpp = Fmt.str "%a" Stan_math_code_gen.pp_prog opt_mir in
        let uninit_warnings =
          if is_flag_set "warn-uninitialized" then
            Pedantic_analysis.warn_uninitialized mir
          else [] in
        let pedantic_warnings =
          if is_flag_set "warn-pedantic" then
            Pedantic_analysis.warn_pedantic mir
          else [] in
        (cpp, warnings, uninit_warnings @ pedantic_warnings) in
      match result with
      | Result.Ok (cpp, warnings, pedantic_mode_warnings) ->
          (Result.Ok cpp, warnings, pedantic_mode_warnings)
      | Result.Error _ as e -> (e, parser_warnings, []) )

let wrap_result ?printed_filename ~warnings = function
  | Result.Ok s ->
      Js.Unsafe.obj
        [| ("result", Js.Unsafe.inject (Js.string s))
         ; ( "warnings"
           , Js.Unsafe.inject
               (Js.array (List.to_array (List.map ~f:Js.string warnings))) )
        |]
  | Error e ->
      let e = Fmt.str "%a" (Errors.pp ?printed_filename) e in
      Js.Unsafe.obj
        [| ("errors", Js.Unsafe.inject (Array.map ~f:Js.string [|e|]))
         ; ("warnings", Js.Unsafe.inject Js.array_empty) |]

let stan2cpp_wrapped name code (flags : Js.string_array Js.t Js.opt) =
  let flags =
    let to_ocaml_str_array a =
      Js.(str_array a |> to_array |> Array.map ~f:to_string) in
    Js.Opt.map flags to_ocaml_str_array |> Js.Opt.to_option in
  let is_flag_set =
    match flags with
    | Some flags -> fun flag -> Array.mem ~equal:String.equal flags flag
    | None -> fun _ -> false in
  let flag_val flag =
    let prefix = flag ^ "=" in
    Option.(
      flags
      >>= fun flags ->
      Array.find flags ~f:(String.is_prefix ~prefix)
      >>= String.chop_prefix ~prefix) in
  let printed_filename = flag_val "filename-in-msg" in
  let result, warnings, pedantic_mode_warnings =
    stan2cpp (Js.to_string name) (Js.to_string code) is_flag_set flag_val in
  let warnings =
    List.map
      ~f:(Fmt.str "%a" (Warnings.pp ?printed_filename))
      (warnings @ pedantic_mode_warnings) in
  wrap_result ?printed_filename result ~warnings

let dump_stan_math_signatures () =
  Js.string @@ Fmt.str "%a" Stan_math_signatures.pretty_print_all_math_sigs ()

let () =
  Js.export "dump_stan_math_signatures" dump_stan_math_signatures ;
  Js.export "stanc" stan2cpp_wrapped
