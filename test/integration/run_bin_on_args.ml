module Caml_unix = Unix
open Core

let string_of_status = function
  | Caml_unix.WEXITED i -> sprintf "[exit %n]" i
  | WSIGNALED i -> sprintf "[signal %n]" i
  | WSTOPPED i -> sprintf "[stopped %n]" i

let run_capturing_output cmd =
  let noflags = Array.create ~len:0 "" in
  let stdout, stdin, stderr = Caml_unix.open_process_full cmd noflags in
  let chns = [stdout; stderr] in
  let out = List.map ~f:In_channel.input_lines chns |> List.concat in
  let status =
    string_of_status (Caml_unix.close_process_full (stdout, stdin, stderr))
  in
  let out = out @ [status] in
  String.concat ~sep:"\n" out

let () =
  let args = Sys.get_argv () in
  let binary = args.(1) in
  let dirs = Array.(sub args ~pos:2 ~len:(length args - 2)) in
  Array.stable_sort ~compare:String.compare dirs;
  Array.iter dirs ~f:(fun arg ->
      let cmd = binary ^ " " ^ arg in
      Printf.printf "  $ %s\n%s\n" cmd (run_capturing_output cmd))
