open Std
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
  | "silent", [] -> (
      match target with
      | Function _ ->
          Error
            (Some
               (fun ppf () ->
                 Fmt.pf ppf
                   "Annotation '%@%s' can only be applied to variables, but \
                    was applied here to a function."
                   a))
      | Variable {block; is_global; _} -> (
          match (block, is_global) with
          | Middle.Block.(Param | TParam | GQuant), true -> Ok ()
          | _ ->
              Error
                (Some
                   (fun ppf () ->
                     Fmt.pf ppf
                       "Annotation '%@%s' can only be to output variables, but \
                        was here applied to a%a variable in the %a block."
                       a
                       (Fmt.if' (not is_global) Fmt.string)
                       " local"
                       (Fmt.of_to_string Middle.Block.block_name)
                       block))))
  | _ -> Error None

let outvar_is_silent Middle.Program.{out_annotations; _} =
  List.exists
    ~f:(fun Middle.Annotation.{name; _} -> name = "silent")
    out_annotations

(** Noisy meaning "not silent" *)
let get_noisy_outvars outvars =
  List.filter ~f:(fun (_, _, outvar) -> not @@ outvar_is_silent outvar) outvars
