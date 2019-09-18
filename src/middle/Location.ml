open Core_kernel

(** Source code locations *)
type t =
  {filename: string; line_num: int; col_num: int; included_from: t option}
[@@deriving sexp, hash, compare]

let pp_context ppf {filename; line_num; col_num; _} =
  try
    let open In_channel in
    let input = create filename in
    for _ = 1 to line_num - 3 do
      ignore (input_line_exn input)
    done ;
    let get_line num =
      if num > 0 then
        match input_line input with
        | Some input -> Printf.sprintf "%6d:  %s\n" num input
        | _ -> ""
      else ""
    in
    let line_2_before = get_line (line_num - 2) in
    let line_before = get_line (line_num - 1) in
    let our_line = get_line line_num in
    let cursor_line = String.make (col_num + 9) ' ' ^ "^\n" in
    let line_after = get_line (line_num + 1) in
    let line_2_after = get_line (line_num + 2) in
    close input ;
    Fmt.pf ppf
      "   -------------------------------------------------\n\
       %s%s%s%s%s%s   -------------------------------------------------\n"
      line_2_before line_before our_line cursor_line line_after line_2_after
  with _ -> ()

(** Return two lines before and after the specified location
    and print a message *)
let pp_with_message ppf (message, loc) =
  Fmt.pf ppf "%a\n%s\n\n" pp_context loc message

let empty = {filename= ""; line_num= 0; col_num= 0; included_from= None}

let rec to_string ?(print_file = true) ?(print_line = true) loc =
  let open Format in
  let file = if print_file then sprintf "'%s', " loc.filename else "" in
  let line = if print_line then sprintf "line %d, " loc.line_num else "" in
  let incl =
    match loc.included_from with
    | Some loc2 -> sprintf ", included from\n%s" (to_string loc2)
    | None -> ""
  in
  sprintf "%s%scolumn %d%s" file line loc.col_num incl

let trim_quotes s =
  let s = String.drop_prefix s 1 in
  String.drop_suffix s 1

let rec of_string_opt str =
  let split_str =
    Str.bounded_split
      (Str.regexp ", line \\|, column \\|, included from\n")
      str 4
  in
  match split_str with
  | [fname; linenum_str; colnum_str] ->
      Some
        { filename= trim_quotes fname
        ; line_num= int_of_string linenum_str
        ; col_num= int_of_string colnum_str
        ; included_from= None }
  | [fname; linenum_str; colnum_str; included_from_str] ->
      of_string_opt included_from_str
      |> Option.map ~f:(fun included_from ->
             { filename= trim_quotes fname
             ; line_num= int_of_string linenum_str
             ; col_num= int_of_string colnum_str
             ; included_from= Some included_from } )
  | _ -> None

let of_position_opt {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  let split_fname =
    Str.bounded_split (Str.regexp ", included from\n") pos_fname 2
  in
  match split_fname with
  | [] -> None
  | [fname] ->
      Some
        { filename= fname
        ; line_num= pos_lnum
        ; col_num= pos_cnum - pos_bol
        ; included_from= None }
  | fname1 :: fname2 :: _ ->
      Option.map (of_string_opt fname2) ~f:(fun included_from ->
          { filename= fname1
          ; line_num= pos_lnum
          ; col_num= pos_cnum - pos_bol
          ; included_from= Some included_from } )
