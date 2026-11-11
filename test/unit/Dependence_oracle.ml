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

let program ~lower ~upper =
  Fmt.str
    "data { int N; } parameters { real a; } model { vector[N] x; real y; for \
     (n in %d:%d) { %a } }"
    lower upper
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

let run_bounds (lower, upper) =
  let statements =
    build_log_prob_dependence_info
      (Test_utils.mir_of_string (program ~lower ~upper)) in
  let writes = paths_of_x statements Write in
  let reads = paths_of_x statements Read in
  let tally =
    List.fold_left
      (List.combine cases (List.combine writes reads))
      ~init:{disjoint= 0; independent= 0; with_distance= 0; refuted_order= 0}
      ~f:(fun tally (case, (write, read)) ->
        List.iter (violations ~lower ~upper case write read) ~f:(fun name ->
            Fmt.pr "%d:%d %a: %s@." lower upper pp_case case name);
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
    "%d:%d cases %d, disjoint %d, proven independent %d, with distance %d, \
     writer never first %d@."
    lower upper (List.length cases) tally.disjoint tally.independent
    tally.with_distance tally.refuted_order

let%expect_test "One loop, linear subscripts, against enumeration" =
  List.iter [(1, 5); (-2, 2); (3, 3); (4, 2)] ~f:run_bounds;
  [%expect
    {|
    1:5 cases 450, disjoint 255, proven independent 16, with distance 36, writer never first 16
    -2:2 cases 450, disjoint 124, proven independent 16, with distance 36, writer never first 16
    3:3 cases 450, disjoint 424, proven independent 16, with distance 36, writer never first 16
    4:2 cases 450, disjoint 450, proven independent 16, with distance 36, writer never first 16
    |}]
