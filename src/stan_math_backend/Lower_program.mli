(** Stan-program-specific C++ generation *)

open Middle

val stanc_args_to_print : string ref

val lower_program :
     ?standalone_functions:bool
  -> ?printed_filename:string
  -> Program.Typed.t
  -> Cpp.program
