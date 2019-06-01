val option_or_else : if_none:'a option -> 'a option -> 'a option

(* Name mangling helper functions for distributions *)
val is_distribution_name : ?infix:string -> string -> bool
val is_propto_distribution : string -> bool
val stdlib_distribution_name : string -> string
val proportional_to_distribution_infix : string
