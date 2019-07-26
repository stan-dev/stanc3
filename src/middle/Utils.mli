(* Name mangling helper functions for distributions *)
val distribution_suffices : string list
val is_distribution_name : ?infix:string -> string -> bool
val is_propto_distribution : string -> bool
val stdlib_distribution_name : string -> string
val proportional_to_distribution_infix : string
val all_but_last_n : 'a list -> int -> 'a list
