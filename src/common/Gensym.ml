let refs : (string * int ref) list ref = ref []
let reset_danger_use_cautiously () = refs := []

let counter prefix =
  assert (not (String.is_empty prefix));
  match List.assoc_opt prefix !refs with
  | Some count -> count
  | None ->
      let count = ref 0 in
      refs := (prefix, count) :: !refs;
      count

let next ~prefix ?(extra = "") () =
  let count = counter prefix in
  incr count;
  Format.sprintf "%s%s%d__" prefix extra !count

let enter ~prefix =
  let old_count = !(counter prefix) in
  (next ~prefix (), fun () -> counter prefix := old_count)
