open Std

let string_of_status = function
  | Unix.WEXITED i -> Printf.sprintf "[exit %n]" i
  | WSIGNALED i -> Printf.sprintf "[signal %n]" i
  | WSTOPPED i -> Printf.sprintf "[stopped %n]" i

let run_capturing_output cmd =
  let noflags = Array.make 0 "" in
  let stdout, stdin, stderr = Unix.open_process_full cmd noflags in
  let chns = [stdout; stderr] in
  let out = List.map ~f:In_channel.input_lines chns |> List.concat in
  let status =
    string_of_status (Unix.close_process_full (stdout, stdin, stderr)) in
  let out = out @ [status] in
  String.concat ~sep:"\n" out

(** Run [cmd] on [file] from the file's own directory, so that a model given as
    [../models/x.stan] is compiled as [x.stan]: the source locations in the
    generated C++ and in debug output, and the command shown in the expected
    file, then do not mention the directory. *)
let run_in_file_dir cmd file =
  let dir = Filename.dirname file in
  if String.equal dir "." then run_capturing_output (cmd ^ " " ^ file)
  else
    let here = Sys.getcwd () in
    (* [%{bin:...}] and [%{exe:...}] may be relative to the build directory *)
    let cmd =
      match String.split_first ~sep:" " cmd with
      | Some (binary, rest) when Filename.is_relative binary ->
          Filename.concat here binary ^ " " ^ rest
      | Some _ | None -> cmd in
    Sys.chdir dir;
    let out = run_capturing_output (cmd ^ " " ^ Filename.basename file) in
    Sys.chdir here;
    out

let () =
  let args = Sys.argv in
  let binary = args.(1) in
  let dirs = Array.(sub args ~pos:2 ~len:(length args - 2)) in
  Array.stable_sort ~cmp:String.compare dirs;
  Array.iter dirs ~f:(fun arg ->
      let arg = String.chop_prefix_if_exists arg ~prefix:"./" in
      let short_cmd =
        (* when displaying the command in the output file, we clean up the
           binary name and show the model by its base name *)
        let binary = String.split_last ~sep:"/" binary |> Option.get |> snd in
        let binary = String.replace_first binary ~sub:".exe" ~by:"" in
        binary ^ " " ^ Filename.basename arg in
      Printf.printf "  $ %s\n%s\n" short_cmd (run_in_file_dir binary arg))
