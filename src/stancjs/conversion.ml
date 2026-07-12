open Core
open Frontend
open Js_of_ocaml

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
  let is_array a = Js.Unsafe.global##._Array##isArray a |> Js.to_bool in
  if not (is_array value) then
    Error
      (Fmt.str
         "Failed to convert stanc.js argument '%s'!@ Array.isArray returned \
          false for value of type '%s'."
         name
         (Js.typeof value |> Js.to_string))
  else Ok (Js.to_array value)

let get_includes_lenient includes : string String.Map.t * string list =
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
        (Fmt.str
           "@[<v>Warning: stanc.js failed to parse included file mapping:@ %s@]")
      warnings )

type flags =
  {name: string; code: string; driver_flags: Driver.Flags.t; color_output: bool}

let process_flags name code (flags : 'a Js.opt) includes :
    (flags, string) result =
  let open Common.Let_syntax.Result in
  let* name = checked_to_string ~name:"name" name in
  let* code = checked_to_string ~name:"code" code in
  let+ flags =
    match Js.Opt.to_option flags with
    | None -> Ok None
    | Some flags ->
        let* flags_array = checked_to_array ~name:"flags" flags in
        let+ ocaml_flags =
          let open Result in
          Array.mapi flags_array ~f:(fun i v ->
              checked_to_string ~name:("flags[" ^ Int.to_string i ^ "]") v)
          |> Array.to_list |> Result.all >>| Array.of_list in
        Driver.Flags.set_backend_args_list
          (ocaml_flags |> Array.to_list |> List.map ~f:(fun o -> "--" ^ o));
        Some ocaml_flags in
  match flags with
  | None ->
      { name
      ; code
      ; driver_flags=
          { Driver.Flags.default with
            include_source= Include_files.InMemory includes }
      ; color_output= false }
  | Some flags ->
      let is_flag_set flag = Array.mem ~equal:String.equal flags flag in
      let flag_val flag =
        let prefix = flag ^ "=" in
        Array.find_map flags ~f:(String.chop_prefix ~prefix) in
      { name
      ; code
      ; driver_flags=
          { optimization_level=
              (let open Analysis_and_optimization in
               if is_flag_set "O0" then Optimize.O0
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
          ; auto_format=
              is_flag_set "auto-format" || is_flag_set "print-canonical"
          ; debug_settings=
              { print_ast= is_flag_set "debug-ast"
              ; print_typed_ast= is_flag_set "debug-typed-ast"
              ; print_mir=
                  (if is_flag_set "debug-mir" then Basic
                   else if is_flag_set "debug-mir-pretty" then Pretty
                   else Off)
              ; print_transformed_mir=
                  (if is_flag_set "debug-transformed-mir" then Basic
                   else if is_flag_set "debug-transformed-mir-pretty" then
                     Pretty
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
              ; debug_data_json=
                  flag_val "debug-data-json"
                  |> Option.map ~f:(fun s -> ("debug-data-json", s)) }
          ; line_length=
              flag_val "max-line-length"
              |> Option.map ~f:Int.of_string
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
      ; color_output= is_flag_set "color-output" }

let str_color ~color_output =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf (if color_output then `Ansi_tty else `None);
  let flush ppf =
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    Buffer.reset buf;
    s in
  Format.kfprintf flush ppf

class type stancReturn = object
  method result : Js.js_string Js.t Js.optdef_prop
  method errors : Js.js_string Js.t Js.js_array Js.t Js.optdef_prop
  method warnings : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop
end

let js_of_warnings warnings =
  warnings |> List.map ~f:Js.string |> Array.of_list |> Js.array

let wrap_error ~warnings e =
  (* NB: The "0" entry is due to a historical mistake that led the first entry
     always being a 0 (this element is a 'tag' used by jsoo internally, but was
     not meant to be exposed to the user). For backward compatibility with
     existing consumers of stanc.js we have to keep this behavior. *)
  let errors = [| Js.string "0"; Js.string e |] |> Js.array in
  object%js
    val result = Js.undefined [@@optdef]
    val errors = Js.def errors [@@optdef]
    val warnings = js_of_warnings warnings
  end

let wrap_result ?printed_filename ~code ~color_output ~warnings res =
  match res with
  | Result.Ok s ->
      object%js
        val result = Js.def (Js.string s) [@@optdef]
        val errors = Js.undefined [@@optdef]
        val warnings = js_of_warnings warnings
      end
  | Error e ->
      let e =
        str_color ~color_output "%a" (Errors.pp ?printed_filename ~code) e in
      wrap_error ~warnings e
