open Core_kernel
module Unix = Caml_unix

let maybe_convert_cmd_to_windows cmd =
  let pattern = "/install/default/bin/" in
  let to_windows str =
    String.substr_replace_first ~pattern ~with_:"/default.windows/" str ^ ".exe"
  in
  let path =
    String.prefix cmd
      (String.substr_index_exn ~pattern cmd + String.length pattern) in
  if Sys.file_exists (to_windows path) then to_windows cmd else cmd

let run_capturing_output cmd =
  let noflags = Array.create ~len:0 "" in
  let stdout, stdin, stderr =
    Unix.open_process_full (maybe_convert_cmd_to_windows cmd) noflags in
  let chns = [stdout; stderr] in
  let out = List.map ~f:In_channel.input_lines chns in
  ignore (Unix.close_process_full (stdout, stdin, stderr)) ;
  String.concat ~sep:"\n" (List.concat out)

let () =
  let binary = Sys.argv.(1) in
  let dirs = Array.(sub Sys.argv ~pos:2 ~len:(length Sys.argv - 2)) in
  Array.stable_sort ~compare:String.compare dirs ;
  Array.iter dirs ~f:(fun arg ->
      let cmd = binary ^ " " ^ arg in
      Printf.printf "  $ %s\n%s\n" cmd (run_capturing_output cmd) )
