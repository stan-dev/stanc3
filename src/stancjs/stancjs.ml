open Std
open Frontend
open Conversion
open Js_of_ocaml

let invoke_driver model_name model flags =
  let warnings = ref [] in
  let compilation_result =
    Return.with_return @@ fun return ->
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
    let open Result.Syntax in
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
        @ List.map ~f:(Warnings.to_grace ?printed_filename ~code) warnings in
      wrap_result ?printed_filename ~color_output ~code result ~warnings
  | Error non_compilation_error (* either an ICE or malformed JS input *) ->
      wrap_error ~color_output:false ~warnings:include_reader_warnings
        non_compilation_error

(** Like [stan2cpp_wrapped] but just checks the model for correctness, doesn't
    return the generated C++. The output is in a rich object form. Throws errors
    on bad input or ICEs *)
let check_model name code flags includes =
  let includes = get_includes includes in
  let {name; code; driver_flags; color_output= _} =
    process_flags name code flags includes |> res_or_throw in
  let driver_flags =
    { driver_flags with
      warn_uninitialized= true
    ; (* Disable certain flags that we know won't have an affect here *)
      auto_format= false
    ; info= false
    ; version= false
    ; debug_settings= Driver.Flags.default.debug_settings } in
  let warnings = ref [] in
  let output : Driver.Entry.other_output -> unit = function
    | Warnings w -> warnings := !warnings @ w
    | _ -> () in
  let result =
    Common.ICE.with_exn_message (fun () ->
        Driver.Entry.stan2cpp name (`Code code) driver_flags output)
    |> res_or_throw in
  let printed_filename = driver_flags.filename_in_msg in
  let warnings =
    List.map ~f:(Warnings.to_grace ?printed_filename ~code) !warnings in
  let errors =
    match result with
    | Ok _ -> []
    | Error e -> [Errors.to_grace ?printed_filename ~code e] in
  object%js
    val errors = json_of_diagnostics errors
    val warnings = json_of_diagnostics warnings
  end

(** Like [stan2cpp_wrapped] but always formats the model, doesn't return the
    generated C++. The output is in a rich object form. Throws errors on bad
    input or ICEs *)
let format_model name code flags includes =
  let includes = get_includes includes in
  let {name; code; driver_flags; color_output= _} =
    process_flags name code flags includes |> res_or_throw in
  let driver_flags =
    { driver_flags with
      auto_format= true
    ; debug_settings= Driver.Flags.default.debug_settings } in
  let warnings = ref [] in
  let run () =
    let res =
      Return.with_return @@ fun return ->
      let output : Driver.Entry.other_output -> unit = function
        | Warnings w -> warnings := !warnings @ w
        | Formatted s -> return (Ok s)
        | _ -> () in
      Driver.Entry.stan2cpp name (`Code code) driver_flags output in
    res in
  let result = Common.ICE.with_exn_message run |> res_or_throw in
  let printed_filename = driver_flags.filename_in_msg in
  let warnings =
    List.map ~f:(Warnings.to_grace ?printed_filename ~code) !warnings in
  let result, errors =
    match result with
    | Ok formatted -> (Js.def (Js.string formatted), [])
    | Error e -> (Js.undefined, [Errors.to_grace ?printed_filename ~code e])
  in
  object%js
    val result = result [@@optdef]
    val errors = json_of_diagnostics errors
    val warnings = json_of_diagnostics warnings
  end

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
  Js.export "stanc" (Js.Unsafe.callback stan2cpp_wrapped);
  Js.export "check_model" (Js.Unsafe.callback check_model);
  Js.export "format_model" (Js.Unsafe.callback format_model)
