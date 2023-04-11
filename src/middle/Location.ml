open Core_kernel
module Str = Re.Str

type t = {filename: string; line_num: int; col_num: int; included_from: t option}
[@@deriving sexp, hash]

let pp_context_list ppf (lines, {line_num; col_num; _}) =
  let advance l =
    let front = List.hd !l in
    match front with
    | Some _ ->
        l := List.tl_exn !l ;
        front
    | None -> None in
  let input = ref lines in
  for _ = 1 to line_num - 3 do
    ignore (advance input : string option)
  done ;
  let get_line num =
    if num > 0 then
      match advance input with
      | Some input -> Printf.sprintf "%6d:  %s\n" num input
      | _ -> ""
    else "" in
  let line_2_before = get_line (line_num - 2) in
  let line_before = get_line (line_num - 1) in
  let our_line = get_line line_num in
  let cursor_line = String.make (col_num + 9) ' ' ^ "^\n" in
  let line_after = get_line (line_num + 1) in
  let line_2_after = get_line (line_num + 2) in
  Fmt.pf ppf
    "   -------------------------------------------------\n\
     %s%s%s%s%s%s   -------------------------------------------------\n"
    line_2_before line_before our_line cursor_line line_after line_2_after

let context_to_string (context_cb : unit -> string list) (loc : t) :
    string option =
  Option.try_with (fun () ->
      Fmt.to_to_string pp_context_list (context_cb (), loc) )

let empty = {filename= ""; line_num= 0; col_num= 0; included_from= None}

let rec to_string ?printed_filename ?(print_file = true) ?(print_line = true)
    loc =
  let open Format in
  let incl, filename =
    match loc.included_from with
    | Some loc2 ->
        ( sprintf ", included from\n%s" (to_string ?printed_filename loc2)
        , loc.filename )
    | None -> ("", Option.value ~default:loc.filename printed_filename) in
  let file = if print_file then sprintf "'%s', " filename else "" in
  let line = if print_line then sprintf "line %d, " loc.line_num else "" in
  sprintf "%s%scolumn %d%s" file line loc.col_num incl

let compare loc1 loc2 =
  let rec unfold = function
    | {included_from= None; _} as loc -> [loc]
    | {included_from= Some loc1; _} as loc2 ->
        (* When pretty-printing comments it is possible to end up with multiple
           locations at an identical point in the file when they originated in an include.
           We artificially break this tie by pretending that included locations
           were included from the one line down from where they truly were.
        *)
        loc2 :: unfold {loc1 with line_num= loc1.line_num + 1} in
  let rec go = function
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | hd1 :: tl1, hd2 :: tl2 ->
        let x = Int.compare hd1.line_num hd2.line_num in
        if x <> 0 then x
        else
          let x = Int.compare hd1.col_num hd2.col_num in
          if x <> 0 then x else go (tl1, tl2) in
  go (List.rev (unfold loc1), List.rev (unfold loc2))

let trim_quotes s =
  let s = String.drop_prefix s 1 in
  String.drop_suffix s 1

let line_str = ", line "
let line_len = String.length line_str
let line_pat = String.Search_pattern.create ~case_sensitive:true line_str
let column_str = ", column "
let column_len = String.length column_str
let column_pat = String.Search_pattern.create ~case_sensitive:true column_str
let included_str = ", included from\n"
let included_len = String.length included_str

let included_pat =
  String.Search_pattern.create ~case_sensitive:true included_str

let rec of_string_opt str =
  let open Option.Monad_infix in
  let line = String.Search_pattern.index line_pat ~in_:str in
  let column = String.Search_pattern.index column_pat ~in_:str in
  Option.both line column
  >>= fun (line, column) ->
  let included_from = String.Search_pattern.index included_pat ~in_:str in
  match included_from with
  | None ->
      Some
        { filename= trim_quotes (String.slice str 0 line)
        ; line_num= int_of_string (String.slice str (line + line_len) column)
        ; col_num= int_of_string (String.slice str (column + column_len) 0)
        ; included_from= None }
  | Some included_idx ->
      of_string_opt (String.slice str (included_idx + included_len) 0)
      >>| fun included_from ->
      { filename= trim_quotes (String.slice str 0 line)
      ; line_num= int_of_string (String.slice str (line + line_len) column)
      ; col_num=
          int_of_string (String.slice str (column + column_len) included_idx)
      ; included_from= Some included_from }

let of_position_opt {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  let split_locations =
    String.Search_pattern.index included_pat ~in_:pos_fname in
  match split_locations with
  | None ->
      Some
        { filename= pos_fname
        ; line_num= pos_lnum
        ; col_num= pos_cnum - pos_bol
        ; included_from= None }
  | Some included_idx ->
      let fname1, fname2 =
        ( String.slice pos_fname 0 included_idx
        , String.slice pos_fname (included_idx + included_len) 0 ) in
      Option.map (of_string_opt fname2) ~f:(fun included_from ->
          { filename= fname1
          ; line_num= pos_lnum
          ; col_num= pos_cnum - pos_bol
          ; included_from= Some included_from } )

let of_position_exn fn = Option.value_exn (of_position_opt fn)

(* -- TESTS ----------------------------------------------------------------- *)
let%expect_test "location string equivalence 1" =
  let str =
    "'xxx.stan', line 245, column 13, included from\n\
     'yyy.stan', line 666, column 42, included from\n\
     'zzz.stan', line 24, column 77" in
  print_endline (to_string @@ Option.value_exn (of_string_opt str)) ;
  [%expect
    {|
      'xxx.stan', line 245, column 13, included from
      'yyy.stan', line 666, column 42, included from
      'zzz.stan', line 24, column 77 |}]

let%expect_test "location string equivalence 2" =
  let loc : t =
    { filename= "xxx.stan"
    ; line_num= 35
    ; col_num= 24
    ; included_from=
        Some
          { filename= "yyy.stan"
          ; line_num= 345
          ; col_num= 214
          ; included_from= None } } in
  print_endline (to_string @@ Option.value_exn (of_string_opt (to_string loc))) ;
  [%expect
    {|
      'xxx.stan', line 35, column 24, included from
      'yyy.stan', line 345, column 214 |}]

let%expect_test "parse location from string" =
  let loc = Option.value_exn (of_string_opt "'xxx.stan', line 245, column 13") in
  print_endline loc.filename ;
  print_endline (string_of_int loc.line_num) ;
  print_endline (string_of_int loc.col_num) ;
  [%expect {|
      xxx.stan
      245
      13 |}]
