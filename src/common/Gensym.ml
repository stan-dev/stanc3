let _counter = ref 0

let generate () =
  _counter := !_counter + 1 ;
  Format.sprintf "sym%d__" !_counter

let enter () =
  let old_counter = !_counter in
  (generate (), fun () -> _counter := old_counter)

let reset_danger_use_cautiously () = _counter := 0
