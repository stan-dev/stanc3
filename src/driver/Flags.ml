type t =
  { (* ------------------------------- *)
    (* flags affecting code generation *)
    optimization_level: Analysis_and_optimization.Optimize.optimization_level
  ; allow_undefined: bool
  ; functions_only: bool
  ; standalone_functions: bool
  ; use_opencl: bool (* ------------------------- *)
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
  { debug_mir: debug_options
  ; debug_transformed_mir: debug_options
  ; debug_optimized_mir: debug_options
  ; debug_mem_patterns: bool (* Note: still outputs C++ *)
  ; debug_lir: bool
  ; debug_generate_data: bool
  ; debug_generate_inits: bool
  ; debug_data_file: string option }

and debug_options = Off | Basic | Pretty

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
      { debug_mir= Off
      ; debug_transformed_mir= Off
      ; debug_optimized_mir= Off
      ; debug_mem_patterns= false
      ; debug_lir= false
      ; debug_generate_data= false
      ; debug_generate_inits= false
      ; debug_data_file= None }
  ; line_length= 78
  ; canonicalizer_settings= Frontend.Canonicalize.none
  ; warn_pedantic= false
  ; warn_uninitialized= false
  ; filename_in_msg= None }
