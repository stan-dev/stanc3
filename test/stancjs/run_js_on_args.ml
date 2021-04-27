open Core

let run_capturing_output cmd =
  let noflags = Array.create ~len:0 "" in
  let channels = Core.Unix.open_process_full cmd ~env:noflags in
  let chns = [channels.stdout; channels.stderr] in
  let out = List.map ~f:In_channel.input_lines chns in
  ignore (Core.Unix.close_process_full channels) ;
  String.concat ~sep:"\n" (List.concat out)

let () =
  let args = Sys.get_argv () in
  let files = Array.(sub args ~pos:1 ~len:(length args - 1)) in
  Array.stable_sort ~compare:String.compare files ;
  Array.iter files ~f:(fun arg ->
      let cmd = "node " ^ arg in
      Printf.printf "$ %s\n%s\n" cmd (run_capturing_output cmd) )