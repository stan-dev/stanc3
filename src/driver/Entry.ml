open Core
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle

let version = "%%NAME%%3 %%VERSION%%"

let fmt_sexp s =
  let ppf = Format.str_formatter in
  Format.pp_set_margin ppf 90;
  Sexp.pp_hum ppf s;
  Format.flush_str_formatter ()

let set_model_name model_name =
  let mangle =
    String.concat_map ~f:(fun c ->
        Char.(
          if is_alphanum c || c = '_' then to_string c
          else match c with '-' -> "_" | _ -> "x" ^ Int.to_string (to_int c)))
  in
  let model_name_munged =
    Flags.remove_dotstan List.(hd_exn (rev (String.split model_name ~on:'/')))
  in
  if String.equal model_name model_name_munged then
    (* model name was not file-like, so we leave as is (e.g. from --name argument) *)
    Typechecker.model_name := mangle model_name
  else
    (* model name was a file-like thing, so we add _model to match existing behavior *)
    Typechecker.model_name := mangle (model_name_munged ^ "_model")

let reset_mutable_states model_name (flags : Flags.t) =
  Common.Gensym.reset_danger_use_cautiously ();
  Include_files.include_provider := flags.include_source;
  set_model_name model_name;
  Typechecker.check_that_all_functions_have_definition :=
    not flags.allow_undefined;
  Transform_Mir.use_opencl := flags.use_opencl;
  Lower_program.standalone_functions :=
    flags.functions_only || flags.standalone_functions

type other_output =
  | Formatted of string
  | DebugOutput of string
  | Memory_patterns of string
  | Info of string
  | Version of string
  | Generated of string
  | Warnings of Warnings.t list

type compilation_result = (string, Errors.t) result

let debug_output_mir output mir = function
  | Flags.Off -> ()
  | Basic ->
      output (DebugOutput (fmt_sexp [%sexp (mir : Middle.Program.Typed.t)]))
  | Pretty -> output (DebugOutput (Fmt.str "%a" Program.Typed.pp mir))

let stan2cpp model_name model (flags : Flags.t) (output : other_output -> unit)
    : compilation_result =
  let open Common.Let_syntax.Result in
  reset_mutable_states model_name flags;
  if flags.version then output (Version (Fmt.str "%s" version));
  let ast, parser_warnings =
    if flags.functions_only then
      Parse.parse Parser.Incremental.functions_only model
    else Parse.parse Parser.Incremental.program model in
  output (Warnings parser_warnings);
  let* result =
    let* ast = ast in
    if flags.debug_settings.print_ast then
      output (DebugOutput (fmt_sexp [%sexp (ast : Ast.untyped_program)]));
    let+ typed_ast, type_warnings =
      Typechecker.check_program ast
      |> Result.map_error ~f:(fun e -> Errors.Semantic_error e) in
    if flags.debug_settings.print_typed_ast then
      output (DebugOutput (fmt_sexp [%sexp (typed_ast : Ast.typed_program)]));
    output (Warnings type_warnings);
    if flags.info then output (Info (Info.info typed_ast));
    let deprecation_warnings =
      if flags.canonicalizer_settings.deprecations then []
      else Deprecation_analysis.collect_warnings typed_ast in
    output (Warnings deprecation_warnings);
    if flags.auto_format then
      output
        (Formatted
           (Pretty_print_prog.pretty_print_typed_program
              ~bare_functions:flags.functions_only
              ~line_length:flags.line_length
              ~inline_includes:flags.canonicalizer_settings.inline_includes
              ~strip_comments:flags.canonicalizer_settings.strip_comments
              (Canonicalize.canonicalize_program typed_ast
                 flags.canonicalizer_settings)));
    let mir = Ast_to_Mir.trans_prog model_name typed_ast in
    if flags.warn_uninitialized then
      output (Warnings (Pedantic_analysis.warn_uninitialized mir));
    if flags.warn_pedantic then
      output (Warnings (Pedantic_analysis.warn_pedantic mir));
    debug_output_mir output mir flags.debug_settings.print_mir;
    let* generation_context =
      match flags.debug_settings.debug_data_json with
      | None -> Ok Map.Poly.empty
      | Some string -> (
          try
            Ok
              (Debug_data_generation.json_to_mir
                 (Ast_to_Mir.gather_declarations typed_ast.datablock)
                 (Yojson.Basic.from_string string))
          with Yojson.Json_error reason ->
            Error
              (Errors.DebugDataError
                 ( Location_span.empty
                 , "Failed to parse data JSON for debug generation: " ^ reason
                 ))) in
    let* () =
      if flags.debug_settings.debug_generate_data then
        let* data =
          Debug_data_generation.gen_values_json ~context:generation_context
            (Ast_to_Mir.gather_declarations typed_ast.datablock) in
        Ok (output (Generated data))
      else Ok () in
    let* () =
      if flags.debug_settings.debug_generate_inits then
        let* inits =
          Debug_data_generation.gen_values_json ~context:generation_context
            (Ast_to_Mir.gather_declarations typed_ast.parametersblock) in
        Ok (output (Generated inits))
      else Ok () in
    let tx_mir = Transform_Mir.trans_prog mir in
    debug_output_mir output tx_mir flags.debug_settings.print_transformed_mir;
    let opt_mir =
      Optimize.optimization_suite
        ~settings:(Flags.get_optimization_settings flags)
        tx_mir in
    if flags.debug_settings.print_mem_patterns then
      output
        (Memory_patterns (Fmt.str "%a" Memory_patterns.pp_mem_patterns opt_mir));
    debug_output_mir output opt_mir flags.debug_settings.print_optimized_mir;
    let cpp =
      Lower_program.lower_program ?printed_filename:flags.filename_in_msg
        opt_mir in
    if flags.debug_settings.print_lir then
      output (DebugOutput (fmt_sexp [%sexp (cpp : Cpp.program)]));
    let cpp_str = Fmt.(to_to_string Cpp.Printing.pp_program) cpp in
    Ok cpp_str in
  result
