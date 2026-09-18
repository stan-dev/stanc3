open Std
open Std.Sexp_conv

type t =
  { filename: string
  ; line_num: int
  ; col_num: int
  ; byte_num: int
  ; included_from: t option }
[@@deriving sexp_of]

let empty =
  {filename= ""; line_num= -1; col_num= -1; byte_num= -1; included_from= None}

(** Format the location for error messaging.

    If printed_filename is passed, it replaces the highest-level name and leaves
    the filenames of included files intact. *)
let rec pp ?(print_file = true) ?(print_line = true) printed_filename ppf loc =
  let incl, filename =
    match loc.included_from with
    | Some loc2 ->
        ( Format.dprintf ", included from\n%a" (pp printed_filename) loc2
        , loc.filename )
    | None -> (ignore, Option.value ~default:loc.filename printed_filename)
  in
  let file =
    Fmt.if' print_file (fun ppf s ->
        Fmt.pf ppf "%a, " Fmt.(styled (`Fg (`Hi `Blue)) (Fmt.fmt "'%s'")) s)
  in
  let line = Fmt.if' print_line (Fmt.fmt "line %d, ") in
  Fmt.pf ppf "%a%acolumn %d%t" file filename line loc.line_num loc.col_num incl

let compare loc1 loc2 =
  let rec unfold = function
    | {included_from= None; _} as loc -> [loc]
    | {included_from= Some loc1; _} as loc2 -> loc2 :: unfold loc1 in
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

let rec initial_file_loc loc =
  match loc.included_from with None -> loc | Some l -> initial_file_loc l
