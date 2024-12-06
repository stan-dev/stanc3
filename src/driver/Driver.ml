open Core
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle

let version = "%%NAME%% %%VERSION%%"

(* See https://ocaml.org/manual/5.2/bindingops.html#ss%3Aletops-conventions
   This is an alternative to the [let%bind] and [let%map] syntax from ppx_let:
   https://blog.janestreet.com/let-syntax-and-why-you-should-use-it/ *)
module Result_let = struct
  let ( let* ) = Result.( >>= )
  let ( let+ ) = Result.( >>| )
end

let reset_mutable_states model_name (flags : Flags.t) =
  Common.Gensym.reset_danger_use_cautiously ();
  Include_files.include_provider := flags.include_source;
  Typechecker.model_name := model_name;
  Typechecker.check_that_all_functions_have_definition :=
    not flags.allow_undefined;
  Transform_Mir.use_opencl := flags.use_opencl;
  Lower_program.standalone_functions :=
    flags.functions_only || flags.standalone_functions

let stan2cpp model_name model_code (flags : Flags.t) :
    (string, Errors.t) result * Warnings.t list =
  reset_mutable_states model_name flags;
  With_return.with_return (fun r ->
      if flags.version then r.return (Result.Ok (Fmt.str "%s" version), []);
      let ast, parser_warnings =
        if flags.functions_only then
          Parse.parse_string Parser.Incremental.functions_only model_code
        else Parse.parse_string Parser.Incremental.program model_code in
      let open Result_let in
      let result =
        let* ast = ast in
        Debugging.ast_logger ast;
        let+ typed_ast, type_warnings =
          Typechecker.check_program ast
          |> Result.map_error ~f:(fun e -> Errors.Semantic_error e) in
        Debugging.typed_ast_logger typed_ast;
        let warnings = parser_warnings @ type_warnings in
        if flags.info then r.return (Result.Ok (Info.info typed_ast), warnings);
        let deprecation_warnings =
          if flags.canonicalizer_settings.deprecations then []
          else Deprecation_analysis.collect_warnings typed_ast in
        let warnings = warnings @ deprecation_warnings in
        if flags.auto_format then
          r.return
            ( Result.Ok
                (Pretty_print_prog.pretty_print_typed_program
                   ~bare_functions:flags.functions_only
                   ~line_length:flags.line_length
                   ~inline_includes:flags.canonicalizer_settings.inline_includes
                   ~strip_comments:flags.canonicalizer_settings.strip_comments
                   (Canonicalize.canonicalize_program typed_ast
                      flags.canonicalizer_settings))
            , warnings );
        let mir = Ast_to_Mir.trans_prog model_name typed_ast in
        let () =
          match flags.debug_settings.debug_mir with
          | Off -> ()
          | Basic ->
              r.return
                ( Result.Ok
                    (Sexp.to_string_hum [%sexp (mir : Middle.Program.Typed.t)])
                , warnings )
          | Pretty ->
              r.return (Result.Ok (Fmt.str "%a" Program.Typed.pp mir), warnings)
        in
        if flags.debug_settings.debug_generate_data then
          r.return
            ( Debug_data_generation.gen_values_json
                (Ast_to_Mir.gather_declarations typed_ast.datablock)
            , warnings );
        if flags.debug_settings.debug_generate_inits then
          r.return
            ( Debug_data_generation.gen_values_json
                (* TODO context from flags.debug.debug_data_file *)
                (Ast_to_Mir.gather_declarations typed_ast.parametersblock)
            , warnings );
        let tx_mir = Transform_Mir.trans_prog mir in
        let () =
          match flags.debug_settings.debug_transformed_mir with
          | Off -> ()
          | Basic ->
              r.return
                ( Result.Ok
                    (Sexp.to_string_hum
                       [%sexp (tx_mir : Middle.Program.Typed.t)])
                , warnings )
          | Pretty ->
              r.return
                (Result.Ok (Fmt.str "%a" Program.Typed.pp tx_mir), warnings)
        in
        let opt_mir =
          Optimize.optimization_suite
            ~settings:(Optimize.level_optimizations flags.optimization_level)
            tx_mir in
        let () =
          match flags.debug_settings.debug_optimized_mir with
          | Off -> ()
          | Basic ->
              r.return
                ( Result.Ok
                    (Sexp.to_string_hum
                       [%sexp (opt_mir : Middle.Program.Typed.t)])
                , warnings )
          | Pretty ->
              r.return
                (Result.Ok (Fmt.str "%a" Program.Typed.pp opt_mir), warnings)
        in
        if flags.debug_settings.debug_mem_patterns then
          r.return
            ( Result.Ok (Fmt.str "%a" Memory_patterns.pp_mem_patterns opt_mir)
            , warnings );
        let cpp =
          Lower_program.lower_program ?printed_filename:flags.filename_in_msg
            opt_mir in
        if flags.debug_settings.debug_lir then
          r.return
            ( Result.Ok (Sexp.to_string_hum [%sexp (cpp : Cpp.program)])
            , warnings );
        let cpp_str = Fmt.(to_to_string Cpp.Printing.pp_program) cpp in
        let uninit_warnings =
          if flags.warn_uninitialized then
            Pedantic_analysis.warn_uninitialized mir
          else [] in
        let pedantic_warnings =
          if flags.warn_pedantic then Pedantic_analysis.warn_pedantic mir
          else [] in
        (cpp_str, warnings @ uninit_warnings @ pedantic_warnings) in
      match result with
      | Result.Ok (cpp, warnings) -> (Result.Ok cpp, warnings)
      | Result.Error e -> (Result.Error e, parser_warnings))
