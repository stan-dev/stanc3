(** Storing locations in the original source *)

open Core_kernel

(**/**)

module Str = Re.Str

(**/**)

(** Source code locations *)
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

(** Turn the given location into a string holding the code of that location.
    Code is retrieved by calling context_cb, which may do IO.
    Exceptions in the callback or in the creation of the string
    (possible if the context is incorrectly too short for the given location)
    return [None] *)
let context_to_string (context_cb : unit -> string list) (loc : t) :
    string option =
  Option.try_with (fun () ->
      Fmt.to_to_string pp_context_list (context_cb (), loc) )

let empty = {filename= ""; line_num= 0; col_num= 0; included_from= None}

(**
Format the location for error messaging.

If printed_filename is passed, it replaces the highest-level name and
leaves the filenames of included files intact.
*)
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
