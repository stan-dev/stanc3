let next (count : int ref) ~prefix =
  incr count;
  Format.sprintf "%ssym%d__" prefix !count

let enter (count : int ref) ~prefix =
  let old_count = !count in
  (next count ~prefix, fun () -> count := old_count)
