(** stanc console application *)
open Core

open Frontend

let write filename data =
  try Out_channel.write_all filename ~data
  with Sys_error msg ->
    Fmt.epr "Error writing to file '%s': %s@." filename msg;
    exit 1

let print_or_write_and_exit output_file data =
  if not (String.equal output_file "") then write output_file data
  else print_endline data;
  exit 0

let print_and_exit data =
  print_endline data;
  exit 0

let output_callback output_file printed_filename :
    Driver.Entry.other_output -> unit = function
  | Info s -> print_and_exit s
  | Version s ->
      (* note: in practice, handled by Cmdliner for this driver *)
      print_and_exit (s ^ " (" ^ Sys.os_type ^ ")")
  | Generated s | Formatted s ->
      (* these options will use the --o flag if it was passed *)
      print_or_write_and_exit output_file s
  | DebugOutput s | Memory_patterns s ->
      (* historically, these flags didn't prevent you from continuing *)
      print_string s
  | Warnings ws -> Warnings.pp_warnings Fmt.stderr ?printed_filename ws

let main () =
  let CLI.
        { debug_lex
        ; debug_parse
        ; print_cpp
        ; name
        ; output_file
        ; model_file
        ; tty_colors
        ; flags } =
    match CLI.parse_or_exit () with
    (* Deal with multiple modalities *)
    | DumpMathSigs ->
        Stan_math_signatures.pretty_print_all_math_sigs Format.std_formatter ();
        exit 0
    | DumpMathDists ->
        Stan_math_signatures.pretty_print_all_math_distributions
          Format.std_formatter ();
        exit 0
    | Compile settings -> settings in
  Fmt_tty.setup_std_outputs ?style_renderer:tty_colors ();
  Debugging.lexer_logging := debug_lex;
  Debugging.grammar_logging := debug_parse;
  Driver.Flags.set_backend_args_list
    (* remove executable itself from list before passing *)
    (Sys.get_argv () |> Array.to_list |> List.tl_exn);
  (* if we only have functions, always compile as standalone *)
  let flags =
    if String.is_suffix model_file ~suffix:".stanfunctions" then
      {flags with standalone_functions= true; functions_only= true}
    else flags in
  let model_file_name, model_source, printed_filename =
    if String.equal model_file "-" then
      ( "stdin"
      , `Code (In_channel.input_all In_channel.stdin)
      , Option.first_some flags.filename_in_msg (Some "stdin") )
    else (model_file, `File model_file, flags.filename_in_msg) in
  match
    Driver.Entry.stan2cpp
      (Option.value ~default:model_file_name name)
      model_source flags
      (output_callback output_file printed_filename)
  with
  | Ok cpp_str ->
      if print_cpp then print_endline cpp_str;
      let out =
        if String.equal output_file "" then
          Driver.Flags.remove_dotstan model_file_name ^ ".hpp"
        else output_file in
      write out cpp_str
  | Error (DebugDataError _ as e) ->
      (* separated out to suggest the possibly-fixing flag *)
      Errors.pp Fmt.stderr ?printed_filename e;
      if Option.is_none flags.debug_settings.debug_data_json then
        Fmt.pf Fmt.stderr "Supplying a --debug-data-file may help@;";
      exit 1
  | Error e ->
      (match model_source with
      | `File _ -> Errors.pp Fmt.stderr ?printed_filename e
      | `Code code -> Errors.pp Fmt.stderr ?printed_filename ~code e);
      exit 1

let () =
  match Common.ICE.with_exn_message main with
  | Ok () -> ()
  | Error internal_error ->
      Out_channel.output_string stderr internal_error;
      Out_channel.flush stderr;
      exit 125
