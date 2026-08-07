open StdLabels
open MoreLabels
open Middle

type state_t = Location_span.t list
type map_rect_registration_t = (int * string) list

let no_span_num = 0

let prepare_prog (mir : Program.Typed.t) :
    Program.Numbered.t * state_t * map_rect_registration_t * bool =
  let label_locations = Queue.create () in
  let map_rect_calls = Queue.create () in
  let location_to_label = Hashtbl.create 64 in
  let needs_mix_header = ref false in
  Queue.push (no_span_num, Location_span.empty) label_locations;
  Hashtbl.replace location_to_label ~key:Location_span.empty ~data:no_span_num;
  (* turn locations into numbers for array printing *)
  let number_meta meta =
    match Hashtbl.find_opt location_to_label meta with
    | Some i -> i
    | None ->
        let new_label = Queue.length label_locations in
        Queue.push (new_label, meta) label_locations;
        Hashtbl.replace location_to_label ~key:meta ~data:new_label;
        new_label in
  let rec number_locations_stmt ({pattern; meta} : Stmt.Located.t) :
      Stmt.Numbered.t =
    let pattern =
      Stmt.Pattern.map number_map_rect_calls_expr number_locations_stmt pattern
    in
    let meta = number_meta meta in
    {meta; pattern}
  (* map_rect numbering *)
  and number_map_rect_calls_expr ({meta; pattern} : Expr.Typed.t) : Expr.Typed.t
      =
    let pattern = Expr.Pattern.map number_map_rect_calls_expr pattern in
    match pattern with
    | FunApp
        ( StanLib ("map_rect", suffix, mem_pattern)
        , ({pattern= Var f; _} :: _ as es) ) ->
        let next_map_rect_id = Queue.length map_rect_calls + 1 in
        Queue.push
          (next_map_rect_id, f ^ Lower_expr.functor_suffix)
          map_rect_calls;
        let pattern =
          Expr.Pattern.FunApp
            ( StanLib ("map_rect", suffix, mem_pattern)
            , List.map ~f:number_map_rect_calls_expr
                (Expr.Helpers.int next_map_rect_id :: es) ) in
        {meta; pattern}
    | FunApp (StanLib (name, _, _), _)
      when (not !needs_mix_header)
           && Stan_math_signatures.is_embedded_laplace_fn name ->
        needs_mix_header := true;
        {meta; pattern}
    | _ -> {meta; pattern} in
  let mir =
    Program.map number_map_rect_calls_expr number_locations_stmt number_meta mir
  in
  let location_list =
    List.map ~f:snd
      (List.sort
         ~cmp:(fun x y -> Int.compare (fst x) (fst y))
         (List.of_seq (Queue.to_seq label_locations))) in
  let map_rect_calls_list =
    List.sort
      ~cmp:(fun (x1, x2) (y1, y2) ->
        match Int.compare x1 y1 with 0 -> String.compare x2 y2 | x -> x)
      (List.of_seq (Queue.to_seq map_rect_calls)) in
  (mir, location_list, map_rect_calls_list, !needs_mix_header)

let gen_globals ?printed_filename location_list =
  let open Cpp in
  let location_list =
    " (found before start of program)"
    :: (List.filter ~f:(fun x -> x <> Location_span.empty) location_list
       |> List.map ~f:(fun x ->
           " (in " ^ Location_span.to_string ?printed_filename x ^ ")"))
    |> List.map ~f:Exprs.literal_string in
  let location_count = List.length location_list in
  let arr_type = Types.const_char_array location_count in
  [ GlobalVariableDefn
      (make_variable_defn ~type_:(TypeLiteral "stan::math::profile_map")
         ~name:"profiles__" ())
  ; GlobalVariableDefn
      (make_variable_defn ~static:true ~constexpr:true ~type_:arr_type
         ~name:"locations_array__"
         ~init:(Assignment (ArrayLiteral location_list)) ()) ]

let assign_loc location_num =
  let open Cpp in
  let open Cpp.DSL in
  if location_num = no_span_num then []
  else ["current_statement__" := Literal (Int.to_string location_num)]

let register_map_rect_functors namespace map_rect_calls =
  let register_functor (i, f) =
    Cpp.Preprocessor
      (MacroApply
         ("STAN_REGISTER_MAP_RECT", [Int.to_string i; namespace ^ "::" ^ f]))
  in
  List.map ~f:register_functor map_rect_calls
