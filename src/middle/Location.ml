open Core

type t = {filename: string; line_num: int; col_num: int; included_from: t option}
[@@deriving sexp, hash]

let pp_context ppf (context_cb, {line_num; col_num; _}) =
  let yellow = `Fg (`Hi `Yellow) in
  let bars =
    Fmt.styled `Faint
    @@ Fmt.any "   -------------------------------------------------" in
  let pp_line_number ppf num =
    let pp ppf n = Fmt.pf ppf "%6d:" n in
    let style = if num = line_num then yellow else `Faint in
    Fmt.styled style pp ppf num in
  let pp_line_and_number =
    let pp ppf (line, num) = Fmt.pf ppf "%a  %s\n" pp_line_number num line in
    Fmt.option pp in
  try
    let advance l =
      let front = List.hd !l in
      match front with
      | Some _ ->
          l := List.tl_exn !l;
          front
      | None -> None in
    let input = ref (context_cb ()) in
    for _ = 1 to line_num - 3 do
      ignore (advance input : string option)
    done;
    let get_line num =
      if num > 0 then
        match advance input with Some input -> Some (input, num) | _ -> None
      else None in
    let line_2_before = get_line (line_num - 2) in
    let line_before = get_line (line_num - 1) in
    let our_line = get_line line_num in
    let cursor_line =
      let rendered_line = Fmt.str "%a" pp_line_and_number our_line in
      let offset = 9 + col_num in
      let copied = Int.min offset (String.length rendered_line) in
      let blank_line =
        String.slice rendered_line 0 copied
        |> String.map ~f:(function '\t' -> '\t' | _ -> ' ') in
      fun ppf () ->
        Fmt.pf ppf "%s%a\n"
          (blank_line ^ String.make (offset - copied) ' ')
          Fmt.(styled yellow char)
          '^' in
    let line_after = get_line (line_num + 1) in
    let line_2_after = get_line (line_num + 2) in
    Fmt.pf ppf "%a\n%a%a%a%a%a%a%a\n" bars () pp_line_and_number line_2_before
      pp_line_and_number line_before pp_line_and_number our_line cursor_line ()
      pp_line_and_number line_after pp_line_and_number line_2_after bars ()
  with _ -> ()

let empty = {filename= ""; line_num= 0; col_num= 0; included_from= None}

(**
Format the location for error messaging.

If printed_filename is passed, it replaces the highest-level name and
leaves the filenames of included files intact.
*)
let rec pp ?printed_filename ?(print_file = true) ?(print_line = true) () ppf
    loc =
  let incl, filename =
    match loc.included_from with
    | Some loc2 ->
        ( (fun ppf () ->
            Fmt.pf ppf ", included from\n%a" (pp ?printed_filename ()) loc2)
        , loc.filename )
    | None -> (Fmt.nop, Option.value ~default:loc.filename printed_filename)
  in
  let file =
    Fmt.if' print_file (fun ppf s ->
        Fmt.pf ppf "%a, "
          Fmt.(styled (`Fg (`Hi `Blue)) (fun ppf s -> Fmt.pf ppf "'%s'" s))
          s) in
  let line = Fmt.if' print_line (fun ppf d -> Fmt.pf ppf "line %d, " d) in
  Fmt.pf ppf "%a%acolumn %d%a" file filename line loc.line_num loc.col_num incl
    ()

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
