(** stanc console application *)
open Core

open Frontend

let exit_ok = CLI.exit_ok
let exit_err = CLI.exit_err

let dump_math_sigs () =
  Stan_math_signatures.pretty_print_all_math_sigs Format.std_formatter ();
  exit_ok

let dump_math_dists () =
  Stan_math_signatures.pretty_print_all_math_distributions Format.std_formatter
    ();
  exit_ok

let write filename data =
  try
    Out_channel.write_all filename ~data;
    exit_ok
  with Sys_error msg ->
    Fmt.epr "Error writing to file '%s': %s@." filename msg;
    exit_err

let print_and_exit data =
  print_endline data;
  exit_ok

let print_or_write_and_exit output_file data =
  if not (String.equal output_file "") then write output_file data
  else print_and_exit data

let output_callback break output_file printed_filename :
    Driver.Entry.other_output -> unit = function
  | Info s -> break (print_and_exit s)
  | Version s ->
      (* note: in practice, handled by Cmdliner for this driver *)
      break (print_and_exit (s ^ " (" ^ Sys.os_type ^ ")"))
  | Generated s | Formatted s ->
      (* these options will use the --o flag if it was passed *)
      break (print_or_write_and_exit output_file s)
  | DebugOutput s | Memory_patterns s ->
      (* historically, these flags didn't prevent you from continuing *)
      print_string s
  | Warnings ws -> Warnings.pp_warnings Fmt.stderr ?printed_filename ws

let stanc ?tty_colors ?(debug_lex : bool = false) ?(debug_parse : bool = false)
    ?(print_cpp : bool = false) ?name ~output_file ~model_file
    (flags : Driver.Flags.t) =
  Fmt_tty.setup_std_outputs ?style_renderer:tty_colors ();
  Debugging.lexer_logging := debug_lex;
  Debugging.grammar_logging := debug_parse;
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
  With_return.with_return @@ fun {return} ->
  match
    Driver.Entry.stan2cpp
      (Option.value ~default:model_file_name name)
      model_source flags
      (output_callback return output_file printed_filename)
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
      exit_err
  | Error e ->
      (match model_source with
      | `File _ -> Errors.pp Fmt.stderr ?printed_filename e
      | `Code code -> Errors.pp Fmt.stderr ?printed_filename ~code e);
      exit_err

(** Deal with multiple modalities.
    [Cmdliner.Cmd.groups] would probably be preferable, but
    doesn't allow for subcommands that look like options
    (i.e., start with [--]) *)
let dispatch_commands args =
  let go () =
    match args with
    | `DumpMathSigs -> dump_math_sigs ()
    | `DumpMathDists -> dump_math_dists ()
    | `Default
        CLI.
          { debug_lex
          ; debug_parse
          ; print_cpp
          ; name
          ; output_file
          ; model_file
          ; tty_colors
          ; flags } ->
        stanc ?tty_colors ~debug_lex ~debug_parse ~print_cpp ?name ~output_file
          ~model_file flags in
  match Common.ICE.with_exn_message go with
  | Ok code -> code
  | Error internal_error ->
      Out_channel.output_string stderr internal_error;
      Out_channel.flush stderr;
      CLI.exit_ice

let main () =
  let open Cmdliner in
  let stanc_cmd = Cmd.v CLI.info (Term.map dispatch_commands CLI.commands) in
  let argv = Sys.get_argv () in
  Driver.Flags.set_backend_args_list
    (* remove executable itself from list before passing *)
    (argv |> Array.to_list |> List.tl_exn);
  (* workaround the fact that single letter flags must be - in CmdLiner *)
  Array.map_inplace argv ~f:(fun s ->
      if String.equal s "--O" then "--O1" else s);
  Cmd.eval' ~argv ~catch:false stanc_cmd

let () = if !Sys.interactive then () else exit (main ())
