open Core_kernel
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle
open Js_of_ocaml

let version = "%%NAME%% %%VERSION%%"

type stanc_error =
  | RemovedDeprecations of (Location_span.t * string) list
  | ProgramError of Errors.t

let stan2cpp model_name model_string is_flag_set flag_val :
    (string, stanc_error) result
    * Warnings.t list
    * Pedantic_analysis.warning_span list =
  Common.Gensym.reset_danger_use_cautiously () ;
  Typechecker.model_name := model_name ;
  Typechecker.check_that_all_functions_have_definition :=
    not (is_flag_set "allow_undefined" || is_flag_set "allow-undefined") ;
  Transform_Mir.use_opencl := is_flag_set "use-opencl" ;
  Lower_program.standalone_functions :=
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
          if is_flag_set "print-canonical" then Canonicalize.legacy
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
                  | "strip-comments" -> {settings with strip_comments= true}
                  (* this probably never applies to stancjs, but for completion: *)
                  | "includes" -> {settings with inline_includes= true}
                  | _ -> settings in
                List.fold ~f:parse ~init:Canonicalize.none
                  (String.split ~on:',' s) in
        let line_length =
          flag_val "max-line-length"
          |> Option.map ~f:int_of_string
          |> Option.value ~default:78 in
        let deprecation_warnings, deprecation_errors =
          if canonicalizer_settings.deprecations then ([], [])
          else
            ( Deprecation_analysis.collect_warnings typed_ast
            , Deprecation_removals.collect_removals typed_ast ) in
        let warnings = warnings @ deprecation_warnings in
        if not (List.is_empty deprecation_errors) then
          r.return
            (Result.Error (RemovedDeprecations deprecation_errors), warnings, []) ;
        if is_flag_set "auto-format" || is_flag_set "print-canonical" then
          r.return
            ( Result.Ok
                (Pretty_printing.pretty_print_typed_program
                   ~bare_functions:(is_flag_set "functions-only")
                   ~line_length
                   ~inline_includes:canonicalizer_settings.inline_includes
                   ~strip_comments:canonicalizer_settings.strip_comments
                   (Canonicalize.canonicalize_program typed_ast
                      canonicalizer_settings ) )
            , warnings
            , [] ) ;
        let mir = Ast_to_Mir.trans_prog model_name typed_ast in
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
            ( Result.map_error
                ~f:(fun e -> ProgramError (Errors.DebugDataError e))
                (Debug_data_generation.gen_values_json
                   (Ast_to_Mir.gather_declarations typed_ast.datablock) )
            , warnings
            , [] ) ;
        if is_flag_set "debug-generate-inits" then
          r.return
            ( Result.map_error
                ~f:(fun e -> ProgramError (Errors.DebugDataError e))
                (Debug_data_generation.gen_values_json
                   (Ast_to_Mir.gather_declarations typed_ast.parametersblock) )
            , warnings
            , [] ) ;
        let tx_mir = Transform_Mir.trans_prog mir in
        if is_flag_set "debug-transformed-mir" then
          r.return
            ( Result.Ok
                (Sexp.to_string_hum [%sexp (tx_mir : Middle.Program.Typed.t)])
            , warnings
            , [] ) ;
        if is_flag_set "debug-transformed-mir-pretty" then
          r.return
            (Result.Ok (Fmt.str "%a" Program.Typed.pp tx_mir), warnings, []) ;
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
        if is_flag_set "debug-mem-patterns" then
          r.return
            ( Result.Ok (Fmt.str "%a" Memory_patterns.pp_mem_patterns opt_mir)
            , warnings
            , [] ) ;
        if is_flag_set "debug-transformed-mir" then
          r.return
            ( Result.Ok
                (Sexp.to_string_hum [%sexp (tx_mir : Middle.Program.Typed.t)])
            , warnings
            , [] ) ;
        if is_flag_set "debug-transformed-mir-pretty" then
          r.return
            (Result.Ok (Fmt.str "%a" Program.Typed.pp tx_mir), warnings, []) ;
        let cpp =
          Lower_program.lower_program
            ?printed_filename:(flag_val "filename-in-msg")
            opt_mir in
        if is_flag_set "debug-lir" then
          r.return
            ( Result.Ok (Sexp.to_string_hum [%sexp (cpp : Cpp.program)])
            , warnings
            , [] ) ;
        let cpp_str = Fmt.(to_to_string Cpp.Printing.pp_program) cpp in
        let uninit_warnings =
          if is_flag_set "warn-uninitialized" then
            Pedantic_analysis.warn_uninitialized mir
          else [] in
        let pedantic_warnings =
          if is_flag_set "warn-pedantic" then
            Pedantic_analysis.warn_pedantic mir
          else [] in
        (cpp_str, warnings, uninit_warnings @ pedantic_warnings) in
      match result with
      | Result.Ok (cpp, warnings, pedantic_mode_warnings) ->
          (Result.Ok cpp, warnings, pedantic_mode_warnings)
      | Result.Error e -> (Result.Error (ProgramError e), parser_warnings, []) )

let wrap_result ?printed_filename ~code ~warnings res =
  let js_warnings =
    ( "warnings"
    , Js.Unsafe.inject
        (Js.array (List.to_array (List.map ~f:Js.string warnings))) ) in
  match res with
  | Result.Ok s ->
      Js.Unsafe.obj [|("result", Js.Unsafe.inject (Js.string s)); js_warnings|]
  | Error (ProgramError e) ->
      let e =
        Fmt.str "%a"
          (Errors.pp ?printed_filename ?code:(Some (Js.to_string code)))
          e in
      Js.Unsafe.obj
        [| ("errors", Js.Unsafe.inject (Array.map ~f:Js.string [|e|]))
         ; js_warnings |]
  | Error (RemovedDeprecations ls) ->
      let errors =
        Fmt.str "%a" (Deprecation_removals.pp_removals ?printed_filename) ls
      in
      Js.Unsafe.obj
        [| ("errors", Js.Unsafe.inject (Array.map ~f:Js.string [|errors|]))
         ; js_warnings |]

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
  let stanc_args_to_print =
    let sans_model_and_hpp_paths x =
      not
        String.(
          is_suffix ~suffix:".stan" x
          && not (is_prefix ~prefix:"filename-in-msg" x)
          || is_prefix ~prefix:"o=" x) in
    (* Ignore the "--o" arg, the stan file and the binary name (bin/stanc). *)
    flags
    |> Option.map ~f:Array.to_list
    |> Option.value ~default:[]
    |> List.filter ~f:sans_model_and_hpp_paths
    |> List.map ~f:(fun o -> "--" ^ o)
    |> String.concat ~sep:" " in
  Lower_program.stanc_args_to_print := stanc_args_to_print ;
  let result, warnings, pedantic_mode_warnings =
    stan2cpp (Js.to_string name) (Js.to_string code) is_flag_set flag_val in
  let warnings =
    List.map
      ~f:(Fmt.str "%a" (Warnings.pp ?printed_filename))
      (warnings @ pedantic_mode_warnings) in
  wrap_result ?printed_filename ~code result ~warnings

let dump_stan_math_signatures () =
  Js.string @@ Fmt.str "%a" Stan_math_signatures.pretty_print_all_math_sigs ()

let dump_stan_math_distributions () =
  Js.string
  @@ Fmt.str "%a" Stan_math_signatures.pretty_print_all_math_distributions ()

let () =
  Js.export "dump_stan_math_signatures" dump_stan_math_signatures ;
  Js.export "dump_stan_math_distributions" dump_stan_math_distributions ;
  Js.export "stanc" stan2cpp_wrapped
