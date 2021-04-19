open Core

let maybe_convert_cmd_to_windows cmd =
  let pattern = "/install/default/bin/" in
  let to_windows str =
    String.substr_replace_first ~pattern ~with_:"/default.windows/" str
    ^ ".exe"
  in
  let path =
    String.prefix cmd
      (String.substr_index_exn ~pattern cmd + String.length pattern)
  in
  match Sys.file_exists (to_windows path) with 
  | `Yes -> to_windows cmd
  | _ -> cmd

let run_capturing_output cmd =
  let noflags = Array.create ~len:0 "" in
  let channels =
  Core.Unix.open_process_full (maybe_convert_cmd_to_windows cmd) ~env:noflags
  in
  let chns = [channels.stdout; channels.stderr] in
  let out = List.map ~f:In_channel.input_lines chns in
  ignore (Core.Unix.close_process_full channels) ;
  String.concat ~sep:"\n" (List.concat out)

let () =
  let args = Sys.get_argv () in
  let binary = Array.get args 1 in
  let dirs = Array.(sub args ~pos:2 ~len:(length args - 2)) in
  Array.stable_sort ~compare:String.compare dirs ;
  Array.iter dirs ~f:(fun arg ->
      let cmd = binary ^ " " ^ arg in
      Printf.printf "  $ %s\n%s\n" cmd (run_capturing_output cmd) )