open Std
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle

let version = "%%NAME%%3 %%VERSION%%"

let fmt_sexp s =
  let ppf = Format.str_formatter in
  Format.pp_set_margin ppf 90;
  Sexplib0.Sexp.pp_hum ppf s;
  Format.flush_str_formatter ()

let set_model_name model_name =
  let mangle =
    String.concat_map ~sep:"" ~f:(fun c ->
        if Char.Ascii.is_alphanum c || c = '_' then String.of_char c
        else match c with '-' -> "_" | _ -> "x" ^ Int.to_string (Char.code c))
  in
  let model_name_munged =
    Common.Files.remove_dotstan
      List.(hd_exn (rev (String.split_on_char model_name ~sep:'/'))) in
  if String.equal model_name model_name_munged then
    (* model name was not file-like, so we leave as is (e.g. from --name
       argument) *)
    Typechecker.model_name := mangle model_name
  else
    (* model name was a file-like thing, so we add _model to match existing
       behavior *)
    Typechecker.model_name := mangle (model_name_munged ^ "_model")

let reset_mutable_states model_name (flags : Flags.t) =
  Common.Gensym.reset_danger_use_cautiously ();
  Include_files.include_provider := flags.include_source;
  set_model_name model_name

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
      output (DebugOutput (fmt_sexp (Middle.Program.Typed.sexp_of_t mir)))
  | Pretty -> output (DebugOutput (Fmt.str "%a" Program.Typed.pp mir))

let stan2cpp model_name model (flags : Flags.t) (output : other_output -> unit)
    : compilation_result =
  let open Result.Syntax in
  reset_mutable_states model_name flags;
  if flags.version then output (Version (Fmt.str "%s" version));
  let ast, parser_warnings =
    if flags.functions_only then Parse.parse_stanfunctions model
    else Parse.parse_program model in
  output (Warnings parser_warnings);
  let* ast in
  if flags.debug_settings.print_ast then
    output (DebugOutput (fmt_sexp (Ast.sexp_of_untyped_program ast)));
  let* typed_ast, type_warnings =
    Typechecker.check_program ~allow_undefined_functions:flags.allow_undefined
      ast
    |> Result.map_error ~f:(fun e -> Errors.Semantic_error e) in
  if flags.debug_settings.print_typed_ast then
    output (DebugOutput (fmt_sexp (Ast.sexp_of_typed_program typed_ast)));
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
            ~bare_functions:flags.functions_only ~line_length:flags.line_length
            ~inline_includes:flags.canonicalizer_settings.inline_includes
            ~strip_comments:flags.canonicalizer_settings.strip_comments
            (Canonicalize.canonicalize_program typed_ast
               flags.canonicalizer_settings)));
  let mir = Ast_to_Mir.trans_prog model_name typed_ast in
  if flags.warn_uninitialized then
    output (Warnings (Pedantic_analysis.warn_uninitialized mir));
  if flags.warn_pedantic then
    output (Warnings (Pedantic_analysis.warn_pedantic mir));
  if flags.debug_settings.debug_print_factor_graph then
    print_endline
      (Factor_graph.factor_graph_to_dot (Factor_graph.prog_factor_graph mir));
  debug_output_mir output mir flags.debug_settings.print_mir;
  let* generation_context =
    match flags.debug_settings.debug_data_json with
    | None -> Ok String.Map.empty
    | Some (ctx, contents) -> (
        try
          Ok
            (Debug_data_generation.json_to_mir
               (Ast_to_Mir.gather_declarations typed_ast.datablock)
               (Yojson.Basic.from_string contents))
        with Yojson.Json_error reason ->
          Error
            (Errors.DebugDataError
               ( Location_span.empty
               , Fmt.str "@[<v2>Failed to parse %s for debug generation:@ %a@]"
                   ctx Fmt.lines reason
               , true ))) in
  let* () =
    if flags.debug_settings.debug_generate_data then
      let+ data =
        Debug_data_generation.gen_values_json ~context:generation_context
          (Ast_to_Mir.gather_declarations typed_ast.datablock) in
      output (Generated data)
    else Ok () in
  let+ () =
    if flags.debug_settings.debug_generate_inits then
      let+ inits =
        Debug_data_generation.gen_values_json ~context:generation_context
          (Ast_to_Mir.gather_declarations typed_ast.parametersblock) in
      output (Generated inits)
    else Ok () in
  let tx_mir = Transform_Mir.trans_prog ~use_opencl:flags.use_opencl mir in
  debug_output_mir output tx_mir flags.debug_settings.print_transformed_mir;
  let opt_mir =
    Optimize.optimization_suite
      ~settings:(Flags.get_optimization_settings flags)
      tx_mir in
  if flags.debug_settings.print_mem_patterns then
    output
      (Memory_patterns
         (Fmt.str "%a%a@\n" Memory_patterns.pp_mem_patterns opt_mir
            (* TODO should be better associated with the names from above? *)
            Fmt.(list string)
            (Memory_patterns.get_warnings ())));
  debug_output_mir output opt_mir flags.debug_settings.print_optimized_mir;
  let cpp =
    Lower_program.lower_program
      ~standalone_functions:(flags.functions_only || flags.standalone_functions)
      ?printed_filename:flags.filename_in_msg opt_mir in
  if flags.debug_settings.print_lir then
    output (DebugOutput (fmt_sexp (Cpp.sexp_of_program cpp)));
  Fmt.(to_to_string Cpp.Printing.pp_program) cpp
