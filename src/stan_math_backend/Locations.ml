open Core_kernel
open Middle

type loc_t = int

type stmt_num = (mtype_loc_ad, (loc_t sexp_opaque[@compare.ignore])) stmt_with
[@@deriving sexp]

type typed_prog_num = (mtype_loc_ad with_expr, stmt_num) prog [@@deriving sexp]
type state_t = location_span List.t

let no_span_num = 0

let prepare_prog (mir : typed_prog) : typed_prog_num * state_t =
  let module LocSp = struct
    type t = location_span

    let compare = compare_location_span
    let hash = hash_location_span
    let sexp_of_t = sexp_of_location_span
  end in
  let label_to_location = Int.Table.create () in
  let location_to_label = Hashtbl.create (module LocSp) in
  Hashtbl.set label_to_location ~key:no_span_num ~data:no_span ;
  Hashtbl.set location_to_label ~key:no_span ~data:no_span_num ;
  let rec number_locations_stmt ({stmt; smeta} : stmt_loc) : stmt_num =
    let stmt = map_statement (fun x -> x) number_locations_stmt stmt in
    match Hashtbl.find location_to_label smeta with
    | Some i -> {stmt; smeta= i}
    | None ->
        let new_label = Hashtbl.length label_to_location + 1 in
        Hashtbl.set label_to_location ~key:new_label ~data:smeta ;
        Hashtbl.set location_to_label ~key:smeta ~data:new_label ;
        {stmt; smeta= new_label} in
  let mir = map_prog (fun x -> x) number_locations_stmt mir in
  let location_list =
    List.map ~f:snd
      (List.sort
         ~compare:(fun x y -> compare_int (fst x) (fst y))
         (Hashtbl.to_alist label_to_location)) in
  (mir, location_list)

let pp_globals ppf location_list =
  let location_list =
    " (found before start of program)"
    :: ( List.filter ~f:(fun x -> x <> no_span) location_list
       |> List.map ~f:(fun x -> " (in " ^ string_of_location_span x ^ ")") )
  in
  Fmt.pf ppf
    "@ static int current_statement__ = 0;@ static const std::vector<string> \
     locations_array__ = {@[<hov>%a@]};@ "
    Fmt.(list ~sep:comma (fmt "%S"))
    location_list

let pp_smeta ppf location_num =
  if location_num = no_span_num then ()
  else Fmt.pf ppf "current_statement__ = %d;@;" location_num
