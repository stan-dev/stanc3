open Std

let string_of_status = function
  | Unix.WEXITED i -> Printf.sprintf "[exit %n]" i
  | WSIGNALED i -> Printf.sprintf "[signal %n]" i
  | WSTOPPED i -> Printf.sprintf "[stopped %n]" i

(** Run [cmd] and return its stdout lines, stderr lines and exit status. *)
let run_capturing_output cmd =
  let noflags = Array.make 0 "" in
  let stdout, stdin, stderr = Unix.open_process_full cmd noflags in
  let out = In_channel.input_lines stdout in
  let err = In_channel.input_lines stderr in
  let status =
    string_of_status (Unix.close_process_full (stdout, stdin, stderr)) in
  (out, err, status)

(** The command as displayed in the output file, with the binary's directory and
    [.exe] suffix removed *)
let short_cmd binary arg =
  let binary =
    let binary, rest =
      String.split_first ~sep:" " binary |> Option.value ~default:(binary, "")
    in
    let binary = String.split_last ~sep:"/" binary |> Option.get |> snd in
    match rest with "" -> binary | rest -> binary ^ " " ^ rest in
  let binary = String.replace_first binary ~sub:".exe" ~by:"" in
  binary ^ " " ^ arg

(** All output of every command in one block, stderr after stdout *)
let print_combined short_cmd (out, err, status) =
  let lines = out @ err @ [status] in
  Printf.printf "  $ %s\n%s\n" short_cmd (String.concat ~sep:"\n" lines)

(** The command in a leading comment, stdout as is, and stderr with the exit
    status in a trailing comment, so each output file reads as code *)
let print_split short_cmd (out, err, status) =
  let lines =
    [Printf.sprintf "/* $ %s */" short_cmd] @ out @ ["/*"] @ err @ [status; "*/"]
  in
  Printf.printf "%s\n" (String.concat ~sep:"\n" lines)

let () =
  let args = List.tl_exn (Array.to_list Sys.argv) in
  let print, args =
    match args with
    | "--split-output" :: args -> (print_split, args)
    | args -> (print_combined, args) in
  let binary, files =
    match args with
    | binary :: files -> (binary, files)
    | [] -> failwith "Usage: run_bin_on_args [--split-output] binary files..."
  in
  List.stable_sort ~cmp:String.compare files
  |> List.iter ~f:(fun arg ->
      let arg = String.chop_prefix_if_exists arg ~prefix:"./" in
      print (short_cmd binary arg) (run_capturing_output (binary ^ " " ^ arg)))
