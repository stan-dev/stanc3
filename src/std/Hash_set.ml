open MoreLabels

type 'a t = ('a, unit) Hashtbl.t

let create i : 'a t = Hashtbl.create i
let mem = Hashtbl.mem
let add s key = Hashtbl.replace ~data:() ~key s
let iter ~f s = Hashtbl.iter ~f:(fun ~key ~data:() -> f key) s
let length s = Hashtbl.length s
let is_empty s = length s = 0

let union s1 s2 =
  let s = create (length s1 + length s2) in
  Hashtbl.add_seq s (Hashtbl.to_seq s1);
  Hashtbl.add_seq s (Hashtbl.to_seq s2);
  s
