open Core_kernel

let run_capturing_output cmd =
  let noflags = Array.create ~len:0 "" in
  let stdout, stdin, stderr = Unix.open_process_full cmd noflags in
  let chns = [stdout; stderr] in
  let out = List.map ~f:In_channel.input_lines chns in
  ignore (Unix.close_process_full (stdout, stdin, stderr)) ;
  String.concat ~sep:"\n" (List.concat out)

let _ =
  let binary = Sys.argv.(1) in
  let dirs = Array.(sub Sys.argv ~pos:2 ~len:(length Sys.argv - 2)) in
  Array.stable_sort ~compare:String.compare dirs ;
  Array.iter dirs ~f:(fun arg ->
      let cmd = binary ^ " " ^ arg in
      Printf.printf "  $ %s\n%s\n" cmd (run_capturing_output cmd) )
