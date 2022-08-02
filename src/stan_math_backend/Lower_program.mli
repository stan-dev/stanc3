(** Stan-program-specific C++ generation *)

open Middle

val standalone_functions : bool ref
val stanc_args_to_print : string ref
val lower_program : Program.Typed.t -> Cpp.program
