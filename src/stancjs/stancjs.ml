open Core_kernel
open Frontend
open Stan_math_backend
open Analysis_and_optimization
open Middle

let print_warn_uninitialized
    (uninit_vars : (location_span * string) Set.Poly.t) =
  let show_var_info (span, var_name) =
    string_of_location_span span
    ^ ":\n" ^ "  Warning: The variable '" ^ var_name
    ^ "' may not have been initialized.\n"
  in
  let filtered_uninit_vars =
    Set.filter ~f:(fun (span, _) -> span <> no_span) uninit_vars
  in
  Set.iter filtered_uninit_vars ~f:(fun v_info ->
      Out_channel.output_string stderr (show_var_info v_info) )

let stan2cpp model_name model_string =
  let ast =
    try
      let r = Parse.parse_string Parser.Incremental.program model_string in
      match r with
      | Result.Ok ast -> ast
      | Result.Error err ->
          let loc = Parse.syntax_error_location err
          and msg = Parse.syntax_error_message err in
          Errors.report_parsing_error (msg, loc) ;
          exit 1
    with Errors.SyntaxError err ->
      Errors.report_syntax_error err ;
      exit 1
  in
  let typed_ast =
    try
      match Semantic_check.semantic_check_program ast with
      | Result.Ok prog -> prog
      | Result.Error (error :: _) ->
          let loc = Semantic_error.location error
          and msg = (Fmt.to_to_string Semantic_error.pp) error in
          Errors.report_semantic_error (msg, loc) ;
          exit 1
      | _ ->
          Printf.eprintf "The impossible happened" ;
          exit 1
    with Errors.SemanticError err ->
      Errors.report_semantic_error err ;
      exit 1
  in
  let mir = Ast_to_Mir.trans_prog model_name typed_ast in
  let uninitialized_vars =
    Dependence_analysis.mir_uninitialized_variables mir
  in
  print_warn_uninitialized uninitialized_vars ;
  let tx_mir = Transform_Mir.trans_prog mir in
  let cpp = Fmt.strf "%a" Stan_math_code_gen.pp_prog tx_mir in
  cpp

let map2 f (x, y) = (f x, f y)

let wrap2 f s1 s2 =
  let s1, s2 = map2 Js_of_ocaml.Js.to_string (s1, s2) in
  f s1 s2 |> Js_of_ocaml.Js.string

let _ = Js_of_ocaml.Js.export "stanc" (wrap2 stan2cpp)
