(** General compiler settings. *)
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

(** Settings mainly for developers, not users *)
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

val default : t

val get_optimization_settings :
  t -> Analysis_and_optimization.Optimize.optimization_settings

val set_backend_args_list : string list -> unit
(** This is a helper function to set the [model_compile_info] method
    of the generated C++ to contain a copy of the (relevant) compiler flags *)

val remove_dotstan : string -> string
(** Strip '.stan' or '.stanfunctions' from a filename *)
