open Middle

val pp_functions_functors : Format.formatter -> Program.Numbered.t -> unit
(** Pretty-print all user defined functions. Creates functor structs as needed
  *)

val pp_standalone_fun_def :
  string -> Format.formatter -> 'a Program.fun_def -> unit
(** Creates functions outside the model namespaces which only call the ones
   inside the namespaces *)
