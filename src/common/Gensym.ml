let _counters : (string, int) Hashtbl.t = Hashtbl.create 16
let symbol prefix count = Format.sprintf "%ssym%d__" prefix count

let generate ?(prefix : string = "") () =
  let count = 1 + Option.value ~default:0 (Hashtbl.find_opt _counters prefix) in
  Hashtbl.replace _counters prefix count;
  symbol prefix count

let next (counter : int ref) ~prefix =
  incr counter;
  symbol prefix !counter

let enter ~prefix () =
  let old_counter = Hashtbl.find_opt _counters prefix in
  ( generate ~prefix ()
  , fun () ->
      match old_counter with
      | Some count -> Hashtbl.replace _counters prefix count
      | None -> Hashtbl.remove _counters prefix )

let reset_danger_use_cautiously () = Hashtbl.reset _counters
