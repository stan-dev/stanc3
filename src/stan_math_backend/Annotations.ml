open Middle

let recognized_annotation a ty =
  match a with
  | "extern" -> ( match ty with UnsizedType.UFun _ -> `Fine | _ -> `WrongType)
  | "debug_soa" ->
      (* TODO: can we also warn for wrong locations in a general manner?
         This needs to be on something in log_prob's global scope to be meaningful *)
      if UnsizedType.has_mem_pattern ty then `Fine else `WrongType
  | _ -> `Unknown
