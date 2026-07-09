val remove_dotstan : string -> string
(** Strip '.stan' or '.stanfunctions' from a filename *)

val is_stanfunctions : string -> bool
(** Test if a filename ends in '.stanfunctions' *)
