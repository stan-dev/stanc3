open Core_kernel

let run_capturing_output cmd =
  let noflags = Array.create ~len:0 "" in
  let stdout, stdin, stderr = Unix.open_process_full cmd noflags in
  let chns = [stdout; stderr] in
  let out = List.map ~f:In_channel.input_lines chns in
  ignore (Unix.close_process_full (stdout, stdin, stderr));
  String.concat ~sep:"\n" (List.concat out)

let walk_directory_trees dirs select =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map ~f:(Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs when select f -> loop (f :: result) fs
    | _ :: fs -> loop result fs
    | [] -> result
  in
  loop [] dirs

let _ =
  let binary = Sys.argv.(1) in
  let dir_args = Array.(sub Sys.argv ~pos:2 ~len:(length Sys.argv - 2)) in
  let is_stanfile s = String.is_suffix s ~suffix:".stan" in
  let dirs = walk_directory_trees (Array.to_list dir_args) is_stanfile in
  let dirs = List.to_array dirs in
  (* list to array and back again is ugly *)
  Array.stable_sort ~compare:String.compare dirs ;
  Array.iter dirs ~f:(fun arg ->
      let cmd = binary ^ " " ^ arg in
      Printf.printf "  $ %s\n%s\n" cmd (run_capturing_output cmd) )
