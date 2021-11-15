open Core_kernel
module Unix = Caml_unix

let run_capturing_output cmd =
  let noflags = Array.create ~len:0 "" in
  let stdout, stdin, stderr = Unix.open_process_full cmd noflags in
  let chns = [stdout; stderr] in
  let out = List.map ~f:In_channel.input_lines chns in
  ignore (Unix.close_process_full (stdout, stdin, stderr)) ;
  String.concat ~sep:"\n" (List.concat out)

let () =
  let files = Array.(sub Sys.argv ~pos:1 ~len:(length Sys.argv - 1)) in
  Array.stable_sort ~compare:String.compare files ;
  Array.iter files ~f:(fun arg ->
      let cmd = "node " ^ arg in
      Printf.printf "$ %s\n%s\n" cmd (run_capturing_output cmd) )
