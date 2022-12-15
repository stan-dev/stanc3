open Core_kernel
open Core_kernel.Poly
open Middle

type state_t = Location_span.t list
type map_rect_registration_t = (int * string) list

let no_span_num = 0

let prepare_prog (mir : Program.Typed.t) :
    Program.Numbered.t * state_t * map_rect_registration_t =
  let label_to_location = Int.Table.create () in
  let map_rect_calls = Int.Table.create () in
  let location_to_label = Hashtbl.create (module Location_span) in
  Hashtbl.set label_to_location ~key:no_span_num ~data:Location_span.empty ;
  Hashtbl.set location_to_label ~key:Location_span.empty ~data:no_span_num ;
  (* turn locations into numbers for array printing *)
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
  (* map_rect numbering *)
  let rec number_map_rect_calls_expr ({meta; pattern} : Expr.Typed.t) :
      Expr.Typed.t =
    let pattern = Expr.Fixed.Pattern.map number_map_rect_calls_expr pattern in
    match pattern with
    | FunApp
        ( StanLib ("map_rect", suffix, mem_pattern)
        , ({pattern= Var f; _} :: _ as es) ) ->
        let next_map_rect_id = Hashtbl.length map_rect_calls + 1 in
        Hashtbl.add_exn map_rect_calls ~key:next_map_rect_id
          ~data:(f ^ Lower_expr.functor_suffix) ;
        let pattern =
          Expr.Fixed.Pattern.FunApp
            ( StanLib ("map_rect", suffix, mem_pattern)
            , List.map ~f:number_map_rect_calls_expr
                (Expr.Helpers.int next_map_rect_id :: es) ) in
        {meta; pattern}
    | _ -> {meta; pattern} in
  let rec number_map_rect_calls_stmt s =
    Stmt.Fixed.
      { s with
        pattern=
          Pattern.map number_map_rect_calls_expr number_map_rect_calls_stmt
            s.pattern } in
  let mir =
    Program.map number_map_rect_calls_expr number_map_rect_calls_stmt mir in
  let location_list =
    List.map ~f:snd
      (List.sort
         ~compare:(fun x y -> compare_int (fst x) (fst y))
         (Hashtbl.to_alist label_to_location) ) in
  let map_rect_calls_list =
    List.sort ~compare (Hashtbl.to_alist map_rect_calls) in
  (mir, location_list, map_rect_calls_list)

let gen_globals location_list =
  let open Cpp in
  let location_list =
    " (found before start of program)"
    :: ( List.filter ~f:(fun x -> x <> Location_span.empty) location_list
       |> List.map ~f:(fun x -> " (in " ^ Location_span.to_string x ^ ")") )
    |> List.map ~f:Exprs.literal_string in
  let location_count = List.length location_list in
  let arr_type = Types.const_char_array location_count in
  [ GlobalVariableDefn
      (make_variable_defn ~type_:(TypeLiteral "stan::math::profile_map")
         ~name:"profiles__" () )
  ; GlobalVariableDefn
      (make_variable_defn ~static:true ~constexpr:true ~type_:arr_type
         ~name:"locations_array__"
         ~init:(Assignment (ArrayLiteral location_list)) () ) ]

let assign_loc location_num =
  let open Cpp in
  if location_num = no_span_num then []
  else
    [ Expression
        (Assign (Var "current_statement__", Literal (string_of_int location_num))
        ) ]

let register_map_rect_functors namespace map_rect_calls =
  let register_functor (i, f) =
    Cpp.Preprocessor
      (MacroApply
         ("STAN_REGISTER_MAP_RECT", [string_of_int i; namespace ^ "::" ^ f]) )
  in
  List.map ~f:register_functor map_rect_calls
