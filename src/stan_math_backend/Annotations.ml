open Std

let recognized_annotation a args ty =
  match (a, args) with
  | "extern", [] -> (
      match ty with Middle.UnsizedType.UFun _ -> `Fine | _ -> `WrongType)
  | "silent", [] -> (
      match ty with Middle.UnsizedType.UFun _ -> `WrongType | _ -> `Fine)
  | _ -> `Unknown

let outvar_is_silent Middle.Program.{out_annotations; _} =
  List.exists
    ~f:(fun Middle.Annotation.{name; _} -> name = "silent")
    out_annotations

(** Noisy meaning "not silent" *)
let get_noisy_outvars outvars =
  List.filter ~f:(fun (_, _, outvar) -> not @@ outvar_is_silent outvar) outvars
