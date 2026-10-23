(** [runner.ml] runs the provided binary and prints its standard out and stdin
    in a format suitable for our expected_* tests. *)

open Std

let string_of_status = function
  | Unix.WEXITED i -> Printf.sprintf "[exit %n]" i
  | WSIGNALED i -> Printf.sprintf "[signal %n]" i
  | WSTOPPED i -> Printf.sprintf "[stopped %n]" i

let print_run short_cmd (out, err, status) =
  let lines =
    Printf.sprintf "/* $ %s : %s */" short_cmd (string_of_status status)
    ::
    (match (out, err) with
    | [], err -> err
    | out, [] -> out
    | out, err -> out @ ["/*"] @ err @ ["*/"]) in
  Printf.printf "%s\n" (String.concat ~sep:"\n" lines)

let run_capturing_output cmd =
  let noflags = Array.make 0 "" in
  let stdout, stdin, stderr = Unix.open_process_full cmd noflags in
  let out = In_channel.input_lines stdout in
  let err = In_channel.input_lines stderr in
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  (out, err, status)

let () =
  let binary = Sys.argv.(1) in
  let short_binary =
    (* when displaying the command in the output file, we clean up the binary
       name *)
    let binary, rest =
      String.split_first ~sep:" " binary |> Option.value ~default:(binary, "")
    in
    let binary = String.split_last ~sep:"/" binary |> Option.get |> snd in
    let binary = match rest with "" -> binary | rest -> binary ^ " " ^ rest in
    String.replace_first binary ~sub:".exe" ~by:"" in
  let arg = Sys.argv.(2) in
  let arg = String.chop_prefix_if_exists arg ~prefix:"./" in
  let cmd = binary ^ " " ^ arg in
  let short_cmd = short_binary ^ " " ^ arg in
  print_run short_cmd (run_capturing_output cmd)
