open Core

let recognized_annotation a =
  List.mem ["extern"; "silent"] a ~equal:String.equal

let outvar_is_silent Middle.Program.{out_annotations; _} =
  List.mem out_annotations "silent" ~equal:String.equal

(** Noisy meaning "not silent" *)
let get_noisy_outvars outvars =
  List.filter ~f:(fun (_, _, outvar) -> not @@ outvar_is_silent outvar) outvars
