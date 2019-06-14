open Core_kernel
open Middle

type loc_t = int

type stmt_num = (mtype_loc_ad, (loc_t sexp_opaque[@compare.ignore])) stmt_with
[@@deriving sexp]

type typed_prog_num = (mtype_loc_ad with_expr, stmt_num) prog [@@deriving sexp]
type state_t = (loc_t, location_span) Map.Poly.t

let prepare_prog (mir : typed_prog) : typed_prog_num * state_t =
  let module LocSp = struct
    type t = location_span

    let compare = compare_location_span
    let hash = hash_location_span
    let sexp_of_t = sexp_of_location_span
  end in
  let label_to_location = Int.Table.create () in
  let location_to_label = Hashtbl.create (module LocSp) in
  let _ = Hashtbl.add label_to_location ~key:0 ~data:no_span in
  let _ = Hashtbl.add location_to_label ~key:no_span ~data:0 in
  let rec number_locations_stmt ({stmt; smeta} : stmt_loc) : stmt_num =
    let stmt = map_statement (fun x -> x) number_locations_stmt stmt in
    match Hashtbl.find location_to_label smeta with
    | Some i -> {stmt; smeta= i}
    | None ->
        let new_label = Hashtbl.length label_to_location + 1 in
        let _ = Hashtbl.add label_to_location ~key:new_label ~data:smeta in
        let _ = Hashtbl.add location_to_label ~key:smeta ~data:new_label in
        {stmt; smeta= new_label}
  in
  let mir = map_prog (fun x -> x) number_locations_stmt mir in
  let immutable_label_to_location =
    Hashtbl.fold label_to_location ~init:Map.Poly.empty
      ~f:(fun ~key ~data accum -> Map.set accum ~key ~data )
  in
  (mir, immutable_label_to_location)

let pp_globals ppf (mir, location_map) =
  ignore ppf ;
  ignore mir ;
  ignore location_map ;
  raise_s [%sexp ("not yet implemented" : string)]

let pp_smeta ppf location_num =
  Fmt.pf ppf "current_statement__ = %d;@;" location_num

let no_span_num = 0
let _ = prepare_prog
let _ = pp_globals
let _ = pp_smeta
