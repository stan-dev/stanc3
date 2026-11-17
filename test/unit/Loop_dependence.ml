open Std
open Middle
open Analysis_and_optimization.Dataflow_types
open Analysis_and_optimization.Dataflow_utils
module Access = Analysis_and_optimization.Dependence
module Proof = Analysis_and_optimization.Loop_dependence

let analyze source =
  let mir = Test_utils.mir_of_string source in
  let statements =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      Stmt.{pattern= SList mir.log_prob; meta= Location_span.empty} in
  let effects =
    LabelMap.map statements ~f:(fun (pattern, _) ->
        Access.collect_accesses pattern) in
  let defined =
    LabelMap.fold effects ~init:Set.Poly.empty
      ~f:(fun ~key:_ ~data:accesses names ->
        List.fold_left accesses.Access.writes ~init:names
          ~f:(fun collected write ->
            Set.Poly.add write.Access.variable collected)) in
  let context = Proof.create statements (build_controls statements) defined in
  let paths select =
    LabelMap.bindings effects
    |> List.concat_map ~f:(fun (id, accesses) ->
        List.filter_map (select accesses) ~f:(fun location ->
            if
              location.Access.variable = "v"
              && not (List.is_empty location.path)
            then Some (id, location)
            else None)) in
  ( context
  , paths (fun access -> access.Access.writes)
  , paths (fun access -> access.Access.reads) )

let program upper body =
  "data { int N; int k; int j; array[20] int idx; } parameters { real a; } \
   model { vector[100] v; real y; for (n in 1:" ^ upper ^ ") { " ^ body ^ " } }"

let check_pair ~write_first upper write read expected =
  let assignment = "v[" ^ write ^ "] = a;" in
  let read_assignment = "y = v[" ^ read ^ "];" in
  let body =
    if write_first then assignment ^ read_assignment
    else read_assignment ^ assignment in
  match analyze (program upper body) with
  | context, [(writer, write_location)], [(reader, read_location)] ->
      let actual =
        Proof.may_supply_read context writer write_location reader read_location
      in
      if actual <> expected then failwith ("Unexpected dependence: " ^ body)
  | _ -> failwith "Expected one pair of indexed accesses"

let%expect_test
    "Equal strides establish direction, divisibility and trip-count limits" =
  List.iter
    [ ("n", "n", true); ("n", "n + 1", false); ("n", "n - 1", true)
    ; ("N - n", "N - n - 1", false); ("2 * n", "2 * n - 1", false)
    ; ("n + 5", "n", false); ("2 * n", "4 * n + 1", false) ]
    ~f:(fun (write, read, possible) ->
      check_pair ~write_first:true "5" write read possible);
  check_pair ~write_first:false "5" "n" "n" false;
  check_pair ~write_first:false "5" "n" "n - 1" true;
  [%expect {| |}]

let%expect_test "Literal domains constrain fixed-versus-varying accesses" =
  check_pair ~write_first:true "5" "n" "5" true;
  check_pair ~write_first:false "5" "n" "5" false;
  check_pair ~write_first:true "5" "n" "6" false;
  check_pair ~write_first:true "N" "n" "6" true;
  check_pair ~write_first:true "5" "5" "n" true;
  [%expect {| |}]

let%expect_test "Unknown and nonlinear subscripts retain possible dependencies"
    =
  List.iter
    [("idx[n]", "n"); ("n * n", "n + 1"); ("k", "j")]
    ~f:(fun (write, read) -> check_pair ~write_first:false "5" write read true);
  [%expect {| |}]

let%expect_test "Normalization overflow cannot manufacture independence" =
  let big = "(2147483647 * 2147483647)" in
  check_pair ~write_first:true "5" (big ^ " * 2147483647 * n") "n" true;
  check_pair ~write_first:true "5"
    (big ^ " * n + " ^ big)
    (big ^ " * n - " ^ big)
    true;
  let smallest = "((-2147483647 - 1) * 2147483647 + (-2147483647 - 1))" in
  check_pair ~write_first:true "5" ("n + " ^ smallest) "n" true;
  check_pair ~write_first:true "5" (smallest ^ " * n") "2 * n + 1" true;
  [%expect {| |}]

type linear_case =
  {write_stride: int; write_offset: int; read_stride: int; read_offset: int}

let cases =
  List.concat_map (List.range (-2) 3) ~f:(fun write_stride ->
      List.concat_map (List.range (-2) 3) ~f:(fun read_stride ->
          List.concat_map (List.range (-2) 3) ~f:(fun write_offset ->
              List.map (List.range (-2) 3) ~f:(fun read_offset ->
                  {write_stride; write_offset; read_stride; read_offset}))))

let enumerate upper_text =
  let body =
    String.concat ~sep:"\n"
      (List.map cases ~f:(fun case ->
           Fmt.str "v[%d * n + %d] = a; y = v[%d * n + %d];" case.write_stride
             case.write_offset case.read_stride case.read_offset)) in
  let context, writes, reads = analyze (program upper_text body) in
  List.iter
    (List.combine cases (List.combine writes reads))
    ~f:(fun (case, ((writer, write), (reader, read))) ->
      let forward = Proof.may_supply_read context writer write reader read in
      let backward = Proof.may_supply_read context reader read writer write in
      List.iter (List.range 1 6) ~f:(fun i ->
          List.iter (List.range 1 6) ~f:(fun j ->
              if
                (case.write_stride * i) + case.write_offset
                = (case.read_stride * j) + case.read_offset
              then (
                if i <= j && not forward then
                  failwith (Fmt.str "Lost forward overlap at %d,%d" i j);
                if j < i && not backward then
                  failwith (Fmt.str "Lost backward overlap at %d,%d" i j)))))

let%expect_test "Small-loop enumeration checks every permitted execution order"
    =
  enumerate "5";
  enumerate "N";
  [%expect {| |}]

let%expect_test
    "Nested loops order accesses lexicographically and intersect dimensions" =
  let source =
    {|parameters { real a; } model {
      matrix[10, 10] v; real y;
      for (n in 2:8) for (m in 2:8) {
        y = v[n + 1, m - 1]; v[n, m] = a;
        y = v[n - 1, m + 1];
      }
    }|}
  in
  (match analyze source with
  | context, [(writer, write)], [(first_reader, first); (second_reader, second)]
    ->
      assert (
        not (Proof.may_supply_read context writer write first_reader first));
      assert (Proof.may_supply_read context writer write second_reader second)
  | _ -> failwith "Expected two nested-loop reads");
  let source =
    {|parameters { real a; } model {
      matrix[10, 10] v; real y;
      for (n in 2:8) { v[n, n] = a; y = v[n + 1, n - 1]; }
    }|}
  in
  (match analyze source with
  | context, [(writer, write)], [(reader, read)] ->
      assert (not (Proof.may_supply_read context writer write reader read))
  | _ -> failwith "Expected one multidimensional pair");
  [%expect {| |}]

let%expect_test "Nested-loop enumeration validates direction intersections" =
  let offsets = List.range (-1) 2 in
  let cases =
    List.concat_map offsets ~f:(fun wn ->
        List.concat_map offsets ~f:(fun wm ->
            List.concat_map offsets ~f:(fun rn ->
                List.map offsets ~f:(fun rm -> (wn, wm, rn, rm))))) in
  let body =
    String.concat ~sep:"\n"
      (List.map cases ~f:(fun (wn, wm, rn, rm) ->
           Fmt.str "v[n + %d, m + %d] = a; y = v[n + %d, m + %d];" wn wm rn rm))
  in
  let context, writes, reads =
    analyze
      ("parameters { real a; } model { matrix[10, 10] v; real y; for (n in \
        2:4) for (m in 2:4) { " ^ body ^ " } }") in
  let iterations =
    List.concat_map (List.range 2 5) ~f:(fun n ->
        List.map (List.range 2 5) ~f:(fun m -> (n, m))) in
  List.iter
    (List.combine cases (List.combine writes reads))
    ~f:(fun ((wn, wm, rn, rm), ((writer, write), (reader, read))) ->
      let forward = Proof.may_supply_read context writer write reader read in
      let backward = Proof.may_supply_read context reader read writer write in
      List.iter iterations ~f:(fun (i, j) ->
          List.iter iterations ~f:(fun (k, l) ->
              if i + wn = k + rn && j + wm = l + rm then (
                if (i < k || (i = k && j <= l)) && not forward then
                  failwith "Lost nested forward dependence";
                if (k < i || (k = i && l < j)) && not backward then
                  failwith "Lost nested backward dependence"))));
  [%expect {| |}]
