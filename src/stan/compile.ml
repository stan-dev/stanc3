(** Compile the given program. *)

let compile ot = match ot with Some t -> Debug.asts_string t
                             | _ -> "error"
