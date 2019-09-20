open Core_kernel
open Middle

(* type loc_t = int

type stmt_num = (mtype_loc_ad, (loc_t sexp_opaque[@compare.ignore])) stmt_with
[@@deriving sexp]

type typed_prog_num = (mtype_loc_ad with_expr, stmt_num) prog [@@deriving sexp]


 *)

type state_t = Location_span.t list

let no_span_num = 0

let prepare_prog (mir : Program.Typed.t) : Program.Numbered.t * state_t =
  let label_to_location = Int.Table.create () in
  let location_to_label = Hashtbl.create (module Location_span) in
  Hashtbl.set label_to_location ~key:no_span_num ~data:Location_span.empty ;
  Hashtbl.set location_to_label ~key:Location_span.empty ~data:no_span_num ;
  let rec number_locations_stmt ({pattern; meta} : Stmt.Located.t) :
      Stmt.Numbered.t =
    let pattern = Stmt.Fixed.Pattern.map Fn.id number_locations_stmt pattern in
    match Hashtbl.find location_to_label meta with
    | Some i ->
        let meta = Stmt.Numbered.Meta.from_int i in
        {meta; pattern}
    | None ->
        let new_label = Hashtbl.length label_to_location + 1 in
        Hashtbl.set label_to_location ~key:new_label ~data:meta ;
        Hashtbl.set location_to_label ~key:meta ~data:new_label ;
        {pattern; meta= new_label}
  in
  let mir = Program.map Fn.id number_locations_stmt mir in
  let location_list =
    List.map ~f:snd
      (List.sort
         ~compare:(fun x y -> compare_int (fst x) (fst y))
         (Hashtbl.to_alist label_to_location))
  in
  (mir, location_list)

let pp_globals ppf location_list =
  let location_list =
    " (found before start of program)"
    :: ( List.filter ~f:(fun x -> x <> Location_span.empty) location_list
       |> List.map ~f:(fun x -> " (in " ^ Location_span.to_string x ^ ")") )
  in
  Fmt.pf ppf
    "@ static int current_statement__ = 0;@ static const std::vector<string> \
     locations_array__ = {@[<hov>%a@]};@ "
    Fmt.(list ~sep:comma (fmt "%S"))
    location_list

let pp_smeta ppf location_num =
  if location_num = no_span_num then ()
  else Fmt.pf ppf "current_statement__ = %d;@;" location_num
