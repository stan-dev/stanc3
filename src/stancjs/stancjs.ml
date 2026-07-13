open Core
open Frontend
open Conversion
open Js_of_ocaml

let invoke_driver model_name model flags =
  let warnings = ref [] in
  let compilation_result =
    With_return.with_return @@ fun {return} ->
    let output_callback : Driver.Entry.other_output -> unit = function
      | Warnings w -> warnings := !warnings @ w
      | Formatted s
       |DebugOutput s
       |Memory_patterns s
       |Info s
       |Version s
       |Generated s ->
          (* stanc.js will only ever return one output, so we break out
             prematurely *)
          return (Ok s) in
    Driver.Entry.stan2cpp model_name (`Code model) flags output_callback in
  (compilation_result, !warnings)

(** Handle conversion of JS <-> OCaml values invoke driver *)
let stan2cpp_wrapped name code flags includes : stancReturn Js.t =
  let includes, include_reader_warnings = get_includes_lenient includes in
  let compilation_result =
    let open Stdlib.Result.Syntax in
    let* {name; code; driver_flags; color_output} =
      process_flags name code flags includes in
    let+ result, warnings =
      Common.ICE.with_exn_message (fun () ->
          invoke_driver name code driver_flags) in
    (result, warnings, driver_flags.filename_in_msg, code, color_output) in
  match compilation_result with
  | Ok (result, warnings, printed_filename, code, color_output) ->
      let warnings =
        include_reader_warnings
        @ List.map
            ~f:(str_color ~color_output "%a" (Warnings.pp ?printed_filename))
            warnings in
      wrap_result ?printed_filename ~color_output ~code result ~warnings
  | Error non_compilation_error (* either an ICE or malformed JS input *) ->
      wrap_error ~warnings:include_reader_warnings non_compilation_error

let dump_stan_math_signatures () =
  Js.string @@ Fmt.str "%a" Stan_math_signatures.pretty_print_all_math_sigs ()

let dump_stan_math_distributions () =
  Js.string
  @@ Fmt.str "%a" Stan_math_signatures.pretty_print_all_math_distributions ()

let () =
  Js.export "dump_stan_math_signatures"
    (Js.Unsafe.callback dump_stan_math_signatures);
  Js.export "dump_stan_math_distributions"
    (Js.Unsafe.callback dump_stan_math_distributions);
  Js.export "stanc" (Js.Unsafe.callback stan2cpp_wrapped)
