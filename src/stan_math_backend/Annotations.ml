let recognized_annotation a args ty _ ~is_global:_ =
  match (a, args) with
  | "extern", [] -> (
      match ty with
      | Middle.UnsizedType.UFun _ -> `Fine
      | _ ->
          `Wrong
            (fun ppf () ->
              Fmt.pf ppf
                "Annotation '%@%s' can only be applied to functions, but was \
                 applied here to type %a."
                a Middle.UnsizedType.pp ty))
  | _ -> `Unknown
