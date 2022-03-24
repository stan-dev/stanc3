val model_prefix : string
(** A string put in front of model names in the namespace.
    Currently "model_"
  *)

val standalone_functions : bool ref
(** Flag to generate just function code, used in RStan *)

val pp_prog : Format.formatter -> Middle.Program.Typed.t -> unit
(** Print the full C++ for the stan program. *)
