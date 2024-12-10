open Core

type t =
  { (* ------------------------------- *)
    (* flags affecting code generation *)
    optimization_level: Analysis_and_optimization.Optimize.optimization_level
  ; allow_undefined: bool
  ; functions_only: bool
  ; standalone_functions: bool
  ; use_opencl: bool
  ; include_source: Frontend.Include_files.t
        (* ------------------------- *)
        (* flags which switch compiler "modes" *)
  ; info: bool
  ; version: bool
  ; auto_format: bool
  ; debug_settings: debug_settings
        (* ------------------------- *)
        (* flags for formatting mode *)
  ; line_length: int
  ; canonicalizer_settings: Frontend.Canonicalize.canonicalizer_settings
        (* ------------------------- *)
        (* extra settings *)
  ; warn_pedantic: bool
  ; warn_uninitialized: bool
  ; filename_in_msg: string option }

and debug_settings =
  { debug_ast: bool
  ; debug_typed_ast: bool
  ; debug_mir: debug_options
  ; debug_transformed_mir: debug_options
  ; debug_optimized_mir: debug_options
  ; debug_mem_patterns: bool
  ; debug_manual_soa: bool option
        (* if None, do nothing. If Some true, force on, if Some false, force off *)
  ; debug_lir: bool
  ; debug_generate_data: bool
  ; debug_generate_inits: bool
  ; debug_data_json: string option }

and debug_options = Off | Basic | Pretty

let get_optimization_settings
    {optimization_level; debug_settings= {debug_manual_soa; _}; _} =
  let base_optims =
    Analysis_and_optimization.Optimize.level_optimizations optimization_level
  in
  match debug_manual_soa with
  | Some true -> {base_optims with optimize_soa= true}
  | Some false -> {base_optims with optimize_soa= false}
  | None -> base_optims

let default =
  { optimization_level= Analysis_and_optimization.Optimize.O0
  ; allow_undefined= false
  ; functions_only= false
  ; standalone_functions= false
  ; use_opencl= false
  ; include_source= Frontend.Include_files.FileSystemPaths []
  ; info= false
  ; version= false
  ; auto_format= false
  ; debug_settings=
      { debug_ast= false
      ; debug_typed_ast= false
      ; debug_mir= Off
      ; debug_transformed_mir= Off
      ; debug_optimized_mir= Off
      ; debug_mem_patterns= false
      ; debug_manual_soa= None
      ; debug_lir= false
      ; debug_generate_data= false
      ; debug_generate_inits= false
      ; debug_data_json= None }
  ; line_length= 78
  ; canonicalizer_settings= Frontend.Canonicalize.none
  ; warn_pedantic= false
  ; warn_uninitialized= false
  ; filename_in_msg= None }

let set_backend_args_list flags =
  (* Ignore the "--o" arg, the stan file and the binary name (bin/stanc). *)
  let sans_model_and_hpp_paths x =
    not
      String.(
        is_suffix ~suffix:".stan" x
        && not (is_prefix ~prefix:"--filename-in-msg" x)
        || is_prefix ~prefix:"--o=" x) in
  let stanc_args_to_print =
    flags |> List.filter ~f:sans_model_and_hpp_paths |> String.concat ~sep:" "
  in
  Stan_math_backend.Lower_program.stanc_args_to_print := stanc_args_to_print

let remove_dotstan s =
  if String.is_suffix ~suffix:".stanfunctions" s then String.drop_suffix s 14
  else if String.is_suffix ~suffix:".stan" s then String.drop_suffix s 5
  else s
