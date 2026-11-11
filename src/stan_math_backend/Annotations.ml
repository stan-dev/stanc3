let recognized_annotation a args ty =
  match (a, args) with
  | "extern", [] -> (
      match ty with Middle.UnsizedType.UFun _ -> `Fine | _ -> `WrongType)
  | _ -> `Unknown
