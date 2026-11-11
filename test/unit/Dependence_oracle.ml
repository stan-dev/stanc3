open Std
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_types
module Dependence = Analysis_and_optimization.Dependence

type case =
  {write_stride: int; write_offset: int; read_stride: int; read_offset: int}

let cases : case list =
  let strides = List.range (-2) 3 in
  List.concat_map strides ~f:(fun write_stride ->
      List.concat_map strides ~f:(fun read_stride ->
          List.concat_map [0; 1] ~f:(fun write_offset ->
              List.map (List.range (-4) 5) ~f:(fun read_offset ->
                  {write_stride; write_offset; read_stride; read_offset}))))

let pp_subscript ppf (stride, offset) = Fmt.pf ppf "%d * n + %d" stride offset

let pp_case ppf case =
  Fmt.pf ppf "x[%a] = a; y = x[%a];" pp_subscript
    (case.write_stride, case.write_offset)
    pp_subscript
    (case.read_stride, case.read_offset)

let program ~lower ~upper_text =
  Fmt.str
    "data { int N; } parameters { real a; } model { vector[N] x; real y; for \
     (n in %d:%s) { %a } }"
    lower upper_text
    Fmt.(list ~sep:sp pp_case)
    cases

let overlaps ~lower ~upper case : (int * int) list =
  let iterations = List.range lower (upper + 1) in
  List.concat_map iterations ~f:(fun write_iteration ->
      List.filter_map iterations ~f:(fun read_iteration ->
          if
            (case.write_stride * write_iteration) + case.write_offset
            = (case.read_stride * read_iteration) + case.read_offset
          then Some (write_iteration, read_iteration)
          else None))

let paths_of_x (statements : stmt_info LabelMap.t) kind :
    (label list * Dependence.path) list =
  List.concat_map (LabelMap.bindings statements) ~f:(fun (_, statement) ->
      List.filter_map statement.accesses ~f:(fun (access : access) ->
          if
            String.equal access.var "x"
            && Dependence.Accesses.equal_kind access.kind kind
            && not (List.is_empty access.path)
          then Some (statement.loop_ids, access.path)
          else None))

let violations ~lower ~upper case (write : label list * Dependence.path)
    (read : label list * Dependence.path) : string list =
  let pairs = overlaps ~lower ~upper case in
  let loop_id = List.hd_exn (fst write) in
  let forward = Dependence.test_dependence write read in
  let backward = Dependence.test_dependence read write in
  let has_distance dependence ~f =
    match Dependence.known_distance ~loop_id dependence with
    | None -> true
    | Some distance ->
        List.for_all pairs ~f:(fun (write_iteration, read_iteration) ->
            f write_iteration read_iteration = distance) in
  let checks =
    [ ( "independent hides an overlap"
      , Dependence.is_overlap_possible forward || List.is_empty pairs )
    ; ( "distance excludes an overlap"
      , has_distance forward ~f:(fun write_iteration read_iteration ->
            read_iteration - write_iteration) )
    ; ( "order excludes an overlap in the same iteration"
      , Dependence.is_writer_possibly_first ~is_writer_first:true forward
        || not
             (List.exists pairs ~f:(fun (write_iteration, read_iteration) ->
                  write_iteration <= read_iteration)) )
    ; ( "order excludes an overlap in a later iteration"
      , Dependence.is_writer_possibly_first ~is_writer_first:false forward
        || not
             (List.exists pairs ~f:(fun (write_iteration, read_iteration) ->
                  write_iteration < read_iteration)) )
    ; ( "reversed independent hides an overlap"
      , Dependence.is_overlap_possible backward || List.is_empty pairs )
    ; ( "reversed distance excludes an overlap"
      , has_distance backward ~f:(fun write_iteration read_iteration ->
            write_iteration - read_iteration) )
    ; ( "reversed order excludes an overlap"
      , Dependence.is_writer_possibly_first ~is_writer_first:false backward
        || not
             (List.exists pairs ~f:(fun (write_iteration, read_iteration) ->
                  read_iteration < write_iteration)) ) ] in
  List.filter_map checks ~f:(fun (name, is_sound) ->
      if is_sound then None else Some name)

type tally =
  {disjoint: int; independent: int; with_distance: int; refuted_order: int}

let run_bounds ?upper_text (lower, upper) =
  let upper_text = Option.value upper_text ~default:(Int.to_string upper) in
  let statements =
    build_log_prob_dependence_info
      (Test_utils.mir_of_string (program ~lower ~upper_text)) in
  let writes = paths_of_x statements Write in
  let reads = paths_of_x statements Read in
  let tally =
    List.fold_left
      (List.combine cases (List.combine writes reads))
      ~init:{disjoint= 0; independent= 0; with_distance= 0; refuted_order= 0}
      ~f:(fun tally (case, (write, read)) ->
        List.iter (violations ~lower ~upper case write read) ~f:(fun name ->
            Fmt.pr "%d:%s %a: %s@." lower upper_text pp_case case name);
        let dependence = Dependence.test_dependence write read in
        let count is_counted = if is_counted then 1 else 0 in
        let is_overlap_possible = Dependence.is_overlap_possible dependence in
        { disjoint=
            tally.disjoint + count (List.is_empty (overlaps ~lower ~upper case))
        ; independent= tally.independent + count (not is_overlap_possible)
        ; with_distance=
            tally.with_distance
            + count
                (Option.is_some
                   (Dependence.known_distance
                      ~loop_id:(List.hd_exn (fst write))
                      dependence))
        ; refuted_order=
            tally.refuted_order
            + count
                (is_overlap_possible
                && not
                     (Dependence.is_writer_possibly_first ~is_writer_first:true
                        dependence)) }) in
  Fmt.pr
    "%d:%s cases %d, disjoint %d, proven independent %d, with distance %d, \
     writer never first %d@."
    lower upper_text (List.length cases) tally.disjoint tally.independent
    tally.with_distance tally.refuted_order

let%expect_test "One loop, linear subscripts, against enumeration" =
  List.iter [(1, 5); (-2, 2); (3, 3); (4, 2)] ~f:run_bounds;
  [%expect
    {|
    1:5 cases 450, disjoint 255, proven independent 152, with distance 52, writer never first 22
    -2:2 cases 450, disjoint 124, proven independent 122, with distance 52, writer never first 22
    3:3 cases 450, disjoint 424, proven independent 234, with distance 8, writer never first 0
    4:2 cases 450, disjoint 450, proven independent 250, with distance 0, writer never first 0
    |}]

let%expect_test "One loop, unknown upper bound, against enumeration" =
  run_bounds ~upper_text:"N" (1, 8);
  run_bounds ~upper_text:"N" (-2, 5);
  run_bounds ~upper_text:"N" (3, 3);
  [%expect
    {|
    1:N cases 450, disjoint 245, proven independent 150, with distance 54, writer never first 23
    -2:N cases 450, disjoint 105, proven independent 104, with distance 54, writer never first 23
    3:N cases 450, disjoint 424, proven independent 180, with distance 54, writer never first 23
    |}]

type nest_case =
  {write_strides: int * int; read_strides: int * int; read_offset: int}

let nest_cases : nest_case list =
  let strides = [-1; 0; 1; 2] in
  let stride_pairs =
    List.concat_map strides ~f:(fun outer_stride ->
        List.map strides ~f:(fun inner_stride -> (outer_stride, inner_stride)))
  in
  List.concat_map stride_pairs ~f:(fun write_strides ->
      List.concat_map stride_pairs ~f:(fun read_strides ->
          List.map [0; 1] ~f:(fun read_offset ->
              {write_strides; read_strides; read_offset})))

let pp_nest_subscript ppf ((outer_stride, inner_stride), offset) =
  Fmt.pf ppf "%d * n + %d * m + %d" outer_stride inner_stride offset

let pp_nest_case ppf case =
  Fmt.pf ppf "x[%a] = a; y = x[%a];" pp_nest_subscript (case.write_strides, 0)
    pp_nest_subscript
    (case.read_strides, case.read_offset)

let nest_program =
  Fmt.str
    "data { int N; } parameters { real a; } model { vector[N] x; real y; for \
     (n in 1:3) { for (m in 1:3) { %a } } }"
    Fmt.(list ~sep:sp pp_nest_case)
    nest_cases

let nest_overlaps case : ((int * int) * (int * int)) list =
  let iterations =
    List.concat_map [1; 2; 3] ~f:(fun outer_iteration ->
        List.map [1; 2; 3] ~f:(fun inner_iteration ->
            (outer_iteration, inner_iteration))) in
  let element (outer_stride, inner_stride) offset
      (outer_iteration, inner_iteration) =
    (outer_stride * outer_iteration) + (inner_stride * inner_iteration) + offset
  in
  List.concat_map iterations ~f:(fun write_iteration ->
      List.filter_map iterations ~f:(fun read_iteration ->
          if
            element case.write_strides 0 write_iteration
            = element case.read_strides case.read_offset read_iteration
          then Some (write_iteration, read_iteration)
          else None))

let nest_violations case (write : label list * Dependence.path)
    (read : label list * Dependence.path) : string list =
  let pairs = nest_overlaps case in
  let outer_loop_id, inner_loop_id =
    match fst write with
    | [outer_loop_id; inner_loop_id] -> (outer_loop_id, inner_loop_id)
    | _ -> failwith "expected two loops" in
  let dependence = Dependence.test_dependence write read in
  let has_distance ~loop_id ~f =
    match Dependence.known_distance ~loop_id dependence with
    | None -> true
    | Some distance ->
        List.for_all pairs ~f:(fun (write_iteration, read_iteration) ->
            f read_iteration - f write_iteration = distance) in
  let is_implied is_possible ~by =
    is_possible || not (List.exists pairs ~f:by) in
  let checks =
    [ ( "independent hides an overlap"
      , Dependence.is_overlap_possible dependence || List.is_empty pairs )
    ; ("outer distance", has_distance ~loop_id:outer_loop_id ~f:fst)
    ; ("inner distance", has_distance ~loop_id:inner_loop_id ~f:snd)
    ; ( "order, writer first in the body"
      , is_implied
          (Dependence.is_writer_possibly_first ~is_writer_first:true dependence)
          ~by:(fun (write_iteration, read_iteration) ->
            compare write_iteration read_iteration <= 0) )
    ; ( "order, reader first in the body"
      , is_implied
          (Dependence.is_writer_possibly_first ~is_writer_first:false dependence)
          ~by:(fun (write_iteration, read_iteration) ->
            compare write_iteration read_iteration < 0) )
    ; ( "inner loop order, writer first in the body"
      , is_implied
          (Dependence.is_writer_possibly_first_at_loop ~loop_id:inner_loop_id
             ~is_writer_first:true dependence)
          ~by:(fun ((write_outer, write_inner), (read_outer, read_inner)) ->
            write_outer = read_outer && write_inner <= read_inner) )
    ; ( "inner loop order, reader first in the body"
      , is_implied
          (Dependence.is_writer_possibly_first_at_loop ~loop_id:inner_loop_id
             ~is_writer_first:false dependence)
          ~by:(fun ((write_outer, write_inner), (read_outer, read_inner)) ->
            write_outer = read_outer && write_inner < read_inner) )
    ; ( "outer loop order"
      , is_implied
          (Dependence.is_writer_possibly_first_at_loop ~loop_id:outer_loop_id
             ~is_writer_first:false dependence)
          ~by:(fun (write_iteration, read_iteration) ->
            compare write_iteration read_iteration < 0) ) ] in
  List.filter_map checks ~f:(fun (name, is_sound) ->
      if is_sound then None else Some name)

let%expect_test "Two loops, linear subscripts, against enumeration" =
  let statements =
    build_log_prob_dependence_info (Test_utils.mir_of_string nest_program) in
  let writes = paths_of_x statements Write in
  let reads = paths_of_x statements Read in
  let independent =
    List.fold_left
      (List.combine nest_cases (List.combine writes reads))
      ~init:0
      ~f:(fun independent (case, (write, read)) ->
        List.iter (nest_violations case write read) ~f:(fun name ->
            Fmt.pr "%a: %s@." pp_nest_case case name);
        if
          Dependence.is_overlap_possible (Dependence.test_dependence write read)
        then independent
        else independent + 1) in
  Fmt.pr "cases %d, disjoint %d, proven independent %d@."
    (List.length nest_cases)
    (List.length
       (List.filter nest_cases ~f:(fun case ->
            List.is_empty (nest_overlaps case))))
    independent;
  [%expect {| cases 512, disjoint 194, proven independent 32 |}]
