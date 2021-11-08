(** Perform backend-specific transformations of the MIR
such as mangling or renaming of functions which differ in
the library implementation *)

open Middle

val trans_prog : Program.Typed.t -> Program.Typed.t
val is_opencl_var : string -> bool
val use_opencl : bool ref
