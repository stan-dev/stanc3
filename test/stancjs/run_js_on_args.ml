module Caml_unix = Unix
open Base

let run_capturing_output cmd =
  let env = [| "PATH=" ^ (Sys.getenv "PATH" |> Option.value ~default:"") |] in
  let stdout, stdin, stderr = Caml_unix.open_process_full cmd env in
  let chns = [stdout; stderr] in
  let out = List.map ~f:Stdio.In_channel.input_lines chns in
  ignore (Caml_unix.close_process_full (stdout, stdin, stderr));
  String.concat ~sep:"\n" (List.concat out)

let () =
  let args = Sys.get_argv () in
  let files = Array.(sub args ~pos:1 ~len:(length args - 1)) in
  Array.stable_sort ~compare:String.compare files;
  Array.iter files ~f:(fun arg ->
      let arg = String.chop_prefix_if_exists arg ~prefix:"./" in
      let cmd = "node " ^ arg in
      Stdio.printf "$ %s\n%s\n" cmd (run_capturing_output cmd))
