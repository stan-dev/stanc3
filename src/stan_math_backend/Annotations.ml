open Middle.Annotation

let recognized_annotation a args target =
  match (a, args) with
  | "extern", [] -> (
      match target with
      | Function {has_body= false; _} -> Ok ()
      | Function {has_body= true; _} ->
          Error
            (Some
               (fun ppf () ->
                 Fmt.pf ppf
                   "Annotation '%@%s' can only be applied to functions without \
                    a body."
                   a))
      | Variable {type_; _} ->
          Error
            (Some
               (fun ppf () ->
                 Fmt.pf ppf
                   "Annotation '%@%s' can only be applied to functions, but \
                    was applied here to type %a."
                   a Middle.UnsizedType.pp type_)))
  | _ -> Error None
