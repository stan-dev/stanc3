open Core
open Frontend
open Analysis_and_optimization
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
          (* stanc.js will only ever return one output,
             so we break out prematurely *)
          return (Ok s) in
    Driver.Entry.stan2cpp model_name (`Code model) flags output_callback in
  (compilation_result, !warnings)

let wrap_warnings ~warnings =
  ( "warnings"
  , Js.Unsafe.inject (Js.array (List.to_array (List.map ~f:Js.string warnings)))
  )

let wrap_error ~warnings e =
  Js.Unsafe.obj
    [| ("errors", Js.Unsafe.inject (Array.map ~f:Js.string [| e |]))
     ; wrap_warnings ~warnings |]

let wrap_result ?printed_filename ~code ~warnings res =
  match res with
  | Result.Ok s ->
      Js.Unsafe.obj
        [| ("result", Js.Unsafe.inject (Js.string s)); wrap_warnings ~warnings
        |]
  | Error e ->
      let e =
        Fmt.str "%a"
          (Errors.pp ?printed_filename ?code:(Some (Js.to_string code)))
          e in
      wrap_error ~warnings e

let typecheck e typ = String.equal (Js.to_string (Js.typeof e)) typ

let bad_arg_message ~name ~expected value =
  Fmt.str
    "Failed to convert stanc.js argument '%s'!@ It had type '%s' instead of \
     '%s'."
    name
    (Js.typeof value |> Js.to_string)
    expected

let checked_to_string ~name value =
  if not (typecheck value "string") then
    Error (bad_arg_message ~name ~expected:"string" value)
  else Ok (Js.to_string value)

let checked_to_array ~name value =
  if
    not
      (Js.to_bool
         (Js.Unsafe.meth_call
            (Js.Unsafe.pure_js_expr "Array")
            "isArray"
            [| Js.Unsafe.inject value |]))
  then
    Error
      (Fmt.str
         "Failed to convert stanc.js argument '%s'!@ Array.isArray returned \
          false for value of type '%s'."
         name
         (Js.typeof value |> Js.to_string))
  else Ok (Js.to_array value)

(** Converts from a [{ [s:string]:string }] JS object type
to an OCaml map, with error messages on bad input. *)
let get_includes includes : string String.Map.t * string list =
  let open Common.Let_syntax.Result in
  let map, warnings =
    match Js.Opt.to_option includes with
    | None -> (String.Map.empty, [] (* normal use: argument not supplied *))
    | Some includes when not (typecheck includes "object") ->
        ( String.Map.empty
        , [bad_arg_message ~name:"includes" ~expected:"object" includes] )
    | Some includes ->
        let keys = Js.object_keys includes |> Js.to_array |> List.of_array in
        let lookup k =
          let value_js = Js.Unsafe.get includes k in
          let k_str = Js.to_string k in
          let+ value_str =
            checked_to_string ~name:("includes[\"" ^ k_str ^ "\"]") value_js
          in
          (k_str, value_str) in
        let alist, warnings = List.map keys ~f:lookup |> List.partition_result in
        ( (* JS objects cannot have duplicate keys *)
          String.Map.of_alist_exn alist
        , warnings ) in
  ( map
  , List.map
      ~f:
        (Fmt.str "Warning: stanc.js failed to parse included file mapping:@ %s")
      warnings )

(** Turn our array of flags into a Driver.Flats.t *)
let process_flags (flags : 'a Js.opt) includes : (Driver.Flags.t, string) result
    =
  let open Result in
  let open Common.Let_syntax.Result in
  let+ flags =
    match Js.Opt.to_option flags with
    | None -> Ok None
    | Some flags ->
        let* flags_array = checked_to_array ~name:"flags" flags in
        let+ ocaml_flags =
          Array.mapi flags_array ~f:(fun i v ->
              checked_to_string ~name:("flags[" ^ string_of_int i ^ "]") v)
          |> Array.to_list |> Result.all >>| Array.of_list in
        Driver.Flags.set_backend_args_list
          (ocaml_flags |> Array.to_list |> List.map ~f:(fun o -> "--" ^ o));
        Some ocaml_flags in
  match flags with
  | None ->
      {Driver.Flags.default with include_source= Include_files.InMemory includes}
  | Some flags ->
      let is_flag_set flag = Array.mem ~equal:String.equal flags flag in
      let flag_val flag =
        let prefix = flag ^ "=" in
        Array.find_map flags ~f:(String.chop_prefix ~prefix) in
      { optimization_level=
          (if is_flag_set "O0" then Optimize.O0
           else if is_flag_set "O1" || is_flag_set "O" then Optimize.O1
           else if is_flag_set "Oexperimental" then Optimize.Oexperimental
           else Optimize.O0)
      ; allow_undefined= is_flag_set "allow-undefined"
      ; functions_only= is_flag_set "functions-only"
      ; standalone_functions= is_flag_set "standalone-functions"
      ; use_opencl= is_flag_set "use-opencl"
      ; include_source= Include_files.InMemory includes
      ; info= is_flag_set "info"
      ; version= is_flag_set "version"
      ; auto_format= is_flag_set "auto-format" || is_flag_set "print-canonical"
      ; debug_settings=
          { print_ast= is_flag_set "debug-ast"
          ; print_typed_ast= is_flag_set "debug-typed-ast"
          ; print_mir=
              (if is_flag_set "debug-mir" then Basic
               else if is_flag_set "debug-mir-pretty" then Pretty
               else Off)
          ; print_transformed_mir=
              (if is_flag_set "debug-transformed-mir" then Basic
               else if is_flag_set "debug-transformed-mir-pretty" then Pretty
               else Off)
          ; print_optimized_mir=
              (if is_flag_set "debug-optimized-mir" then Basic
               else if is_flag_set "debug-optimized-mir-pretty" then Pretty
               else Off)
          ; print_mem_patterns= is_flag_set "debug-mem-patterns"
          ; force_soa= None
          ; print_lir= is_flag_set "debug-lir"
          ; debug_generate_data= is_flag_set "debug-generate-data"
          ; debug_generate_inits= is_flag_set "debug-generate-inits"
          ; debug_data_json= flag_val "debug-data-json" }
      ; line_length=
          flag_val "max-line-length"
          |> Option.map ~f:int_of_string
          |> Option.value ~default:78
      ; canonicalizer_settings=
          (if is_flag_set "print-canonical" then Canonicalize.legacy
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
                   | "includes" -> {settings with inline_includes= true}
                   | _ -> settings in
                 List.fold ~f:parse ~init:Canonicalize.none
                   (String.split ~on:',' s))
      ; warn_pedantic= is_flag_set "warn-pedantic"
      ; warn_uninitialized= is_flag_set "warn-uninitialized"
      ; filename_in_msg= flag_val "filename-in-msg" }

(** Handle conversion of JS <-> OCaml values invoke driver *)
let stan2cpp_wrapped name code flags includes =
  let open Common.Let_syntax.Result in
  let includes, include_reader_warnings = get_includes includes in
  let compilation_result =
    let* name = checked_to_string ~name:"name" name in
    let* code = checked_to_string ~name:"code" code in
    let* driver_flags = process_flags flags includes in
    let+ result, warnings =
      Common.ICE.with_exn_message (fun () ->
          invoke_driver name code driver_flags) in
    (result, warnings, driver_flags.filename_in_msg) in
  match compilation_result with
  | Ok (result, warnings, printed_filename) ->
      let warnings =
        include_reader_warnings
        @ List.map ~f:(Fmt.str "%a" (Warnings.pp ?printed_filename)) warnings
      in
      wrap_result ?printed_filename ~code result ~warnings
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
