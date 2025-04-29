let recognized_annotation a ty =
  match a with
  | "extern" -> (
      match ty with Middle.UnsizedType.UFun _ -> `Fine | _ -> `WrongType)
  | _ -> `Unknown
