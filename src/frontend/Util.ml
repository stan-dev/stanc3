open Core_kernel

let _counter = ref 0

let gensym () =
  _counter := !_counter + 1 ;
  sprintf "sym%d__" !_counter

let gensym_checkpoint () =
  let old_counter = !_counter in
  fun () -> _counter := old_counter

let gensym_enter () =
  let reset = gensym_checkpoint () in
  (gensym (), reset)

let gensym_reset_danger_use_cautiously () = _counter := 0
