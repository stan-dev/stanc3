open Core_kernel
open Core_kernel.Poly
open Middle

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
        let new_label = Hashtbl.length label_to_location in
        Hashtbl.set label_to_location ~key:new_label ~data:meta ;
        Hashtbl.set location_to_label ~key:meta ~data:new_label ;
        {pattern; meta= new_label} in
  let mir = Program.map Fn.id number_locations_stmt mir in
  let location_list =
    List.map ~f:snd
      (List.sort
         ~compare:(fun x y -> compare_int (fst x) (fst y))
         (Hashtbl.to_alist label_to_location) ) in
  (mir, location_list)

let gen_globals location_list =
  let open Cpp in
  let location_list =
    " (found before start of program)"
    :: ( List.filter ~f:(fun x -> x <> Location_span.empty) location_list
       |> List.map ~f:(fun x -> " (in " ^ Location_span.to_string x ^ ")") )
    |> List.map ~f:Exprs.literal_string in
  let location_count = List.length location_list in
  let arr_type = Types.const_char_array location_count in
  [ TopVarDef
      (make_variable_defn ~type_:(Type_literal "stan::math::profile_map")
         ~name:"profiles__" () )
  ; TopVarDef
      (make_variable_defn ~static:true ~constexpr:true ~type_:arr_type
         ~name:"locations_array__"
         ~init:(Assignment (ArrayLiteral location_list)) () ) ]

let create_loc_assignment location_num =
  let open Cpp in
  if location_num = no_span_num then []
  else
    [ Expression
        (Assign (Var "current_statement__", Literal (string_of_int location_num))
        ) ]
