open Core

type t =
  { optimization_level: Analysis_and_optimization.Optimize.optimization_level
  ; allow_undefined: bool
  ; functions_only: bool
  ; standalone_functions: bool
  ; use_opencl: bool
  ; include_source: Frontend.Include_files.t
  ; info: bool
  ; version: bool
  ; auto_format: bool
  ; debug_settings: debug_settings
  ; line_length: int
  ; canonicalizer_settings: Frontend.Canonicalize.canonicalizer_settings
  ; warn_pedantic: bool
  ; warn_uninitialized: bool
  ; filename_in_msg: string option }

and debug_settings =
  { print_ast: bool
  ; print_typed_ast: bool
  ; print_mir: debug_options
  ; print_transformed_mir: debug_options
  ; print_optimized_mir: debug_options
  ; print_mem_patterns: bool
  ; force_soa: bool option
  ; print_lir: bool
  ; debug_generate_data: bool
  ; debug_generate_inits: bool
  ; debug_data_json: string option }

and debug_options = Off | Basic | Pretty

let get_optimization_settings
    {optimization_level; debug_settings= {force_soa; _}; _} =
  let base_optims =
    Analysis_and_optimization.Optimize.level_optimizations optimization_level
  in
  match force_soa with
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
      { print_ast= false
      ; print_typed_ast= false
      ; print_mir= Off
      ; print_transformed_mir= Off
      ; print_optimized_mir= Off
      ; print_mem_patterns= false
      ; force_soa= None
      ; print_lir= false
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
  Option.first_some
    (String.chop_suffix ~suffix:".stanfunctions" s)
    (String.chop_suffix ~suffix:".stan" s)
  |> Option.value ~default:s
