open Core_kernel
open Core_kernel.Poly
open Middle

type state_t = Location_span.t list
type map_rect_registration_t = (int * string) list

let no_span_num = 0

let prepare_prog (mir : Program.Typed.t) :
    Program.Numbered.t * state_t * map_rect_registration_t =
  let label_locations = Queue.create () in
  let map_rect_calls = Queue.create () in
  let location_to_label = Hashtbl.create (module Location_span) in
  Queue.enqueue label_locations (no_span_num, Location_span.empty) ;
  Hashtbl.set location_to_label ~key:Location_span.empty ~data:no_span_num ;
  (* turn locations into numbers for array printing *)
  let number_meta meta =
    match Hashtbl.find location_to_label meta with
    | Some i -> i
    | None ->
        let new_label = Queue.length label_locations in
        Queue.enqueue label_locations (new_label, meta) ;
        Hashtbl.set location_to_label ~key:meta ~data:new_label ;
        new_label in
  let rec number_locations_stmt ({pattern; meta} : Stmt.Located.t) :
      Stmt.Numbered.t =
    let pattern =
      Stmt.Fixed.Pattern.map number_map_rect_calls_expr number_locations_stmt
        pattern in
    let meta = number_meta meta in
    {meta; pattern}
  (* map_rect numbering *)
  and number_map_rect_calls_expr ({meta; pattern} : Expr.Typed.t) : Expr.Typed.t
      =
    let pattern = Expr.Fixed.Pattern.map number_map_rect_calls_expr pattern in
    match pattern with
    | FunApp
        ( StanLib ("map_rect", suffix, mem_pattern)
        , ({pattern= Var f; _} :: _ as es) ) ->
        let next_map_rect_id = Queue.length map_rect_calls + 1 in
        Queue.enqueue map_rect_calls
          (next_map_rect_id, f ^ Lower_expr.functor_suffix) ;
        let pattern =
          Expr.Fixed.Pattern.FunApp
            ( StanLib ("map_rect", suffix, mem_pattern)
            , List.map ~f:number_map_rect_calls_expr
                (Expr.Helpers.int next_map_rect_id :: es) ) in
        {meta; pattern}
    | _ -> {meta; pattern} in
  let mir =
    Program.map number_map_rect_calls_expr number_locations_stmt number_meta mir
  in
  let location_list =
    List.map ~f:snd
      (List.sort
         ~compare:(fun x y -> compare_int (fst x) (fst y))
         (Queue.to_list label_locations) ) in
  let map_rect_calls_list = List.sort ~compare (Queue.to_list map_rect_calls) in
  (mir, location_list, map_rect_calls_list)

let gen_globals ?printed_filename location_list =
  let open Cpp in
  let location_list =
    " (found before start of program)"
    :: ( List.filter ~f:(fun x -> x <> Location_span.empty) location_list
       |> List.map ~f:(fun x ->
              " (in " ^ Location_span.to_string ?printed_filename x ^ ")" ) )
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
