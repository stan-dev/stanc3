(** Responsible for turning the Stan function's block
into forward decls, functions, and functors in C++ *)

open Middle

val collect_functors_functions : Program.Numbered.t -> Cpp.defn list
val lower_standalone_fun_def : string -> 'a Program.fun_def -> Cpp.defn list
