open Std
open Middle
open Analysis_and_optimization.Dataflow_types
open Analysis_and_optimization.Dataflow_utils
module Access = Analysis_and_optimization.Dependence

let statement_map source =
  let mir = Test_utils.mir_of_string source in
  let bodies =
    List.filter_map mir.functions_block ~f:(fun definition ->
        definition.Program.fdbody) in
  build_statement_map
    (fun Stmt.{pattern; _} -> pattern)
    (fun Stmt.{meta; _} -> meta)
    Stmt.{meta= Location_span.empty; pattern= SList (bodies @ mir.log_prob)}

let assignment_accesses source =
  statement_map source |> LabelMap.bindings
  |> List.filter_map ~f:(fun (_, (pattern, _)) ->
      match pattern with
      | Stmt.Pattern.Assignment _ -> Some (Access.collect_accesses pattern)
      | _ -> None)

let pp_index_expr ppf (expr : Expr.Typed.t) =
  match Expr.Helpers.array_literal_elements expr with
  | Some elements ->
      Fmt.pf ppf "{%a}" (Fmt.list ~sep:Fmt.comma Expr.Typed.pp) elements
  | None -> Expr.Typed.pp ppf expr

let location_text (accessed_location : Access.location) =
  let index_text index = Fmt.str "[%a]" (Index.pp pp_index_expr) index in
  accessed_location.variable
  ^ String.concat ~sep:"" (List.map accessed_location.path ~f:index_text)

let print_locations heading locations =
  let locations_text =
    locations |> List.map ~f:location_text
    |> List.sort ~cmp:String.compare
    |> String.concat ~sep:", " in
  Fmt.pr "%s: %s@." heading locations_text

let%expect_test "Collect indexed reads, writes, tuple fields and slices" =
  assignment_accesses
    {|model {
        vector[4] v; vector[4] w; array[4] int idx;
        tuple(vector[4], real) t;
        v[idx[1]] = w[idx[2] * idx[3]];
        t.1[idx[1]] = v[2];
        t.1[1] = t.1[2];
        v[1:2] = w[2:3];
        v[1] += 1;
        real z = w[2:3][1];
      }|}
  |> List.iter ~f:(fun statement_accesses ->
      print_locations "reads" statement_accesses.Access.reads;
      print_locations "writes" statement_accesses.writes);
  [%expect
    {|
    reads: idx[1], idx[2], idx[3], w[(idx[2] * idx[3])]
    writes: v[idx[1]]
    reads: idx[1], v[2]
    writes: t[1][idx[1]]
    reads: t[1][2]
    writes: t[1][1]
    reads: w[2:3]
    writes: v[1:2]
    reads: v[1]
    writes: v[1]
    reads: w[2:3]
    writes: z
    |}]

let write_locations source =
  List.concat_map (assignment_accesses source) ~f:(fun statement_accesses ->
      statement_accesses.Access.writes)

let check_overlap first_path second_path expected =
  let actual = Access.may_overlap first_path second_path in
  if actual <> expected then
    Fmt.pr "FAIL: v%a / v%a: expected: %b result: %b@."
      (Index.pp_indices pp_index_expr)
      first_path
      (Index.pp_indices pp_index_expr)
      second_path expected actual
  else
    Fmt.pr "PASS: v%a / v%a@."
      (Index.pp_indices pp_index_expr)
      first_path
      (Index.pp_indices pp_index_expr)
      second_path

let check_locations written_vars source expected_overlap
    expected_first_overwrites_second =
  match write_locations source with
  | [first; second] ->
      check_overlap first.path second.path expected_overlap;
      check_overlap second.path first.path expected_overlap;
      let actual_overwrite = Access.is_overwritten written_vars second first in
      if actual_overwrite <> expected_first_overwrites_second then
        Fmt.pr "Overwrite %s covers %s: expected %b, got %b@."
          (location_text first) (location_text second)
          expected_first_overwrites_second actual_overwrite
  | _ -> failwith "Expected two writes"

let%expect_test "Literal paths are distinct; equal stable paths overwrite" =
  List.iter
    [ ("1", "2", false, false); ("1", "1", true, true); ("k", "k", true, true)
    ; ("k", "j", true, false); ("k + k", "2 * k", true, false)
    ; ("idx[k]", "idx[k]", true, true); ("k * k", "j", true, false) ]
    ~f:(fun (first, second, overlap, overwrite) ->
      let source =
        Fmt.str
          "data { int k; int j; array[4] int idx; } model { vector[4] v; v[%s] \
           = 1; v[%s] = 2; }"
          first second in
      check_locations (Set.Poly.singleton "v") source overlap overwrite);
  [%expect
    {|
    PASS: v[1] / v[2]
    PASS: v[2] / v[1]
    PASS: v[1] / v[1]
    PASS: v[1] / v[1]
    PASS: v[k] / v[k]
    PASS: v[k] / v[k]
    PASS: v[k] / v[j]
    PASS: v[j] / v[k]
    PASS: v[(k + k)] / v[(2 * k)]
    PASS: v[(2 * k)] / v[(k + k)]
    PASS: v[idx[k]] / v[idx[k]]
    PASS: v[idx[k]] / v[idx[k]]
    PASS: v[(k * k)] / v[j]
    PASS: v[j] / v[(k * k)]
    |}]

let%expect_test "Mutable and random indices cannot prove overwrites" =
  check_locations
    (Set.Poly.of_list ["v"; "k"])
    {|data { int k; } model { vector[4] v; v[k] = 1; v[k] = 2; }|} true false;
  check_locations
    (Set.Poly.of_list ["v"; "idx"])
    {|data { int k; array[4] int idx; } model { vector[4] v; v[idx[k]] = 1; v[idx[k]] = 2; }|}
    true false;
  check_locations (Set.Poly.singleton "v")
    {|functions { void f_rng(vector theta) {
        vector[4] v; v[categorical_rng(theta)] = 1; v[categorical_rng(theta)] = 2;
      }} model {}|}
    true false;
  [%expect
    {|
    PASS: v[k] / v[k]
    PASS: v[k] / v[k]
    PASS: v[idx[k]] / v[idx[k]]
    PASS: v[idx[k]] / v[idx[k]]
    PASS: v[categorical_rng(theta)] / v[categorical_rng(theta)]
    PASS: v[categorical_rng(theta)] / v[categorical_rng(theta)]
    |}]

let%expect_test "Tuple fields and whole-variable writes" =
  check_locations Set.Poly.empty
    {|model { tuple(real, real) t; t.1 = 1; t.2 = 2; }|} false false;
  check_locations Set.Poly.empty
    {|model { vector[4] v; v = rep_vector(0, 4); v[1] = 1; }|} true true;
  check_locations Set.Poly.empty
    {|model { vector[4] v; v[1:2] = rep_vector(0, 2); v[3:4] = rep_vector(0, 2); }|}
    false false;
  check_locations Set.Poly.empty
    {|model { array[2] tuple(real, real) t; t[1].2 = 1; t[2].1 = 2; }|} false
    false;
  check_locations Set.Poly.empty
    {|model { array[2] tuple(real, real) t; t[1].1 = 1; t[1].2 = 2; }|} false
    false;
  check_locations Set.Poly.empty
    {|model { array[2] tuple(real, real) t; t[1].2 = 1; t[1].2 = 2; }|} true
    true;
  check_locations Set.Poly.empty
    {|model { tuple(vector[4], real) t; t.1 = rep_vector(0, 4); t.1[2] = 1; }|}
    true true;
  [%expect
    {|
    PASS: v[1] / v[2]
    PASS: v[2] / v[1]
    PASS: v / v[1]
    PASS: v[1] / v
    PASS: v[1:2] / v[3:4]
    PASS: v[3:4] / v[1:2]
    PASS: v[1, 2] / v[2, 1]
    PASS: v[2, 1] / v[1, 2]
    PASS: v[1, 1] / v[1, 2]
    PASS: v[1, 2] / v[1, 1]
    PASS: v[1, 2] / v[1, 2]
    PASS: v[1, 2] / v[1, 2]
    PASS: v[1] / v[1, 2]
    PASS: v[1, 2] / v[1]
    |}]

let%expect_test "Overwrites require the same variable and complete coverage" =
  let locations =
    write_locations
      {|model { vector[4] v; vector[4] w;
          v[1] = 1; v = rep_vector(0, 4); w = rep_vector(0, 4);
        }|}
  in
  (match locations with
  | [element; whole; other] ->
      assert (Access.is_overwritten Set.Poly.empty element whole);
      assert (not (Access.is_overwritten Set.Poly.empty whole element));
      assert (not (Access.is_overwritten Set.Poly.empty element other));
      assert (not (Access.is_overwritten Set.Poly.empty whole other))
  | _ -> failwith "Expected three writes");
  [%expect {| |}]

let%expect_test "Loop headers write the induction variable" =
  let statements =
    statement_map {|data { int N; } model { for (n in 1:N) print(n); }|} in
  LabelMap.iter statements ~f:(fun ~key:_ ~data:(pattern, _) ->
      match pattern with
      | Stmt.Pattern.For _ ->
          let statement_accesses = Access.collect_accesses pattern in
          print_locations "reads" statement_accesses.reads;
          print_locations "writes" statement_accesses.writes
      | _ -> ());
  [%expect {|
    reads: N
    writes: n
    |}]

let%expect_test "Target-modifying calls retain the target read and write" =
  assignment_accesses
    {|functions { real f_lp(real a) { target += a; return a; } }
      parameters { real a; } model { real b = f_lp(a); }|}
  |> List.iter ~f:(fun statement_accesses ->
      print_locations "reads" statement_accesses.Access.reads;
      print_locations "writes" statement_accesses.writes);
  [%expect {|
    reads: a, target
    writes: b, target
    |}]

let check_read_overlap source expected_overlap =
  let reads =
    statement_map source |> LabelMap.bindings
    |> List.concat_map ~f:(fun (_, (pattern, _)) ->
        (Access.collect_accesses pattern).reads)
    |> List.filter ~f:(fun read -> String.equal read.Access.variable "v") in
  match reads with
  | [first; second] ->
      check_overlap first.path second.path expected_overlap;
      check_overlap second.path first.path expected_overlap
  | _ -> failwith "Expected two reads of v"

(* Expected overlap means some valid inputs select a common element. *)
let check_may_idx_overlap first_index second_index ~expected =
  let source =
    Fmt.str
      "data { int k; int j; array[10] int idx; } model { vector[10] v; \
       print(v[%s]); print(v[%s]); }"
      first_index second_index in
  check_read_overlap source expected

let%expect_test "Index overlap: All / All" =
  check_may_idx_overlap ":" ":" ~expected:true;
  [%expect {|
    PASS: v[:] / v[:]
    PASS: v[:] / v[:]
    |}]

let%expect_test "Index overlap: All / Single" =
  check_may_idx_overlap ":" "2" ~expected:true;
  check_may_idx_overlap ":" "1" ~expected:true;
  check_may_idx_overlap ":" "10" ~expected:true;
  check_may_idx_overlap ":" "k" ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[2]
    PASS: v[2] / v[:]
    PASS: v[:] / v[1]
    PASS: v[1] / v[:]
    PASS: v[:] / v[10]
    PASS: v[10] / v[:]
    PASS: v[:] / v[k]
    PASS: v[k] / v[:]
    |}]

let%expect_test "Index overlap: All / Upfrom" =
  check_may_idx_overlap ":" "3:" ~expected:true;
  check_may_idx_overlap ":" "1:" ~expected:true;
  check_may_idx_overlap ":" "10:" ~expected:true;
  check_may_idx_overlap ":" "k:" ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[3:]
    PASS: v[3:] / v[:]
    PASS: v[:] / v[1:]
    PASS: v[1:] / v[:]
    PASS: v[:] / v[10:]
    PASS: v[10:] / v[:]
    PASS: v[:] / v[k:]
    PASS: v[k:] / v[:]
    |}]

let%expect_test "Index overlap: All / Between" =
  check_may_idx_overlap ":" "1:2" ~expected:true;
  check_may_idx_overlap ":" "3:2" ~expected:false;
  check_may_idx_overlap ":" "1:10" ~expected:true;
  check_may_idx_overlap ":" "5:5" ~expected:true;
  check_may_idx_overlap ":" "10:1" ~expected:false;
  check_may_idx_overlap ":" "k:j" ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[1:2]
    PASS: v[1:2] / v[:]
    PASS: v[:] / v[3:2]
    PASS: v[3:2] / v[:]
    PASS: v[:] / v[1:10]
    PASS: v[1:10] / v[:]
    PASS: v[:] / v[5:5]
    PASS: v[5:5] / v[:]
    PASS: v[:] / v[10:1]
    PASS: v[10:1] / v[:]
    PASS: v[:] / v[k:j]
    PASS: v[k:j] / v[:]
    |}]

let%expect_test "Index overlap: All / MultiIndex" =
  check_may_idx_overlap ":" "{1, 3}" ~expected:true;
  check_may_idx_overlap ":" "{1}" ~expected:true;
  check_may_idx_overlap ":" "{10, 1, 10}" ~expected:true;
  check_may_idx_overlap ":" "idx" ~expected:true;
  check_may_idx_overlap ":" "{k, j}" ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[{1, 3}]
    PASS: v[{1, 3}] / v[:]
    PASS: v[:] / v[{1}]
    PASS: v[{1}] / v[:]
    PASS: v[:] / v[{10, 1, 10}]
    PASS: v[{10, 1, 10}] / v[:]
    PASS: v[:] / v[idx]
    PASS: v[idx] / v[:]
    PASS: v[:] / v[{k, j}]
    PASS: v[{k, j}] / v[:]
    |}]

let%expect_test "Index overlap: Single / All" =
  check_may_idx_overlap "2" ":" ~expected:true;
  check_may_idx_overlap "1" ":" ~expected:true;
  check_may_idx_overlap "10" ":" ~expected:true;
  check_may_idx_overlap "k" ":" ~expected:true;
  [%expect
    {|
    PASS: v[2] / v[:]
    PASS: v[:] / v[2]
    PASS: v[1] / v[:]
    PASS: v[:] / v[1]
    PASS: v[10] / v[:]
    PASS: v[:] / v[10]
    PASS: v[k] / v[:]
    PASS: v[:] / v[k]
    |}]

let%expect_test "Index overlap: Single / Single" =
  check_may_idx_overlap "1" "1" ~expected:true;
  check_may_idx_overlap "1" "2" ~expected:false;
  check_may_idx_overlap "2 * k" "2 * k" ~expected:true;
  check_may_idx_overlap "k" "j" ~expected:true;
  (* Even and odd indices cannot select the same element. *)
  check_may_idx_overlap "2 * k" "2 * k + 1" ~expected:false;
  check_may_idx_overlap "10" "10" ~expected:true;
  check_may_idx_overlap "1" "10" ~expected:false;
  check_may_idx_overlap "1" "k" ~expected:true;
  check_may_idx_overlap "k" "k" ~expected:true;
  check_may_idx_overlap "k + 1 * j" "k + 1 * j" ~expected:true;
  [%expect
    {|
    PASS: v[1] / v[1]
    PASS: v[1] / v[1]
    PASS: v[1] / v[2]
    PASS: v[2] / v[1]
    PASS: v[(2 * k)] / v[(2 * k)]
    PASS: v[(2 * k)] / v[(2 * k)]
    PASS: v[k] / v[j]
    PASS: v[j] / v[k]
    PASS: v[(2 * k)] / v[((2 * k) + 1)]
    PASS: v[((2 * k) + 1)] / v[(2 * k)]
    PASS: v[10] / v[10]
    PASS: v[10] / v[10]
    PASS: v[1] / v[10]
    PASS: v[10] / v[1]
    PASS: v[1] / v[k]
    PASS: v[k] / v[1]
    PASS: v[k] / v[k]
    PASS: v[k] / v[k]
    PASS: v[(k + (1 * j))] / v[(k + (1 * j))]
    PASS: v[(k + (1 * j))] / v[(k + (1 * j))]
    |}]

let%expect_test "Index overlap: integer parity" =
  check_may_idx_overlap "2 * k" "2 * j + 1" ~expected:false;
  check_may_idx_overlap "k * 2" "2 * j - 1" ~expected:false;
  check_may_idx_overlap "-(2 * k)" "2 * j + 1" ~expected:false;
  check_may_idx_overlap "2 * k + 1" "2 * j + 3" ~expected:true;
  check_may_idx_overlap "2 * k" "2 * j" ~expected:true;
  check_may_idx_overlap "k * j" "2" ~expected:true;
  check_may_idx_overlap "k * j" "3" ~expected:true;
  check_may_idx_overlap "k + 1" "2 * j" ~expected:true;
  [%expect
    {|
    PASS: v[(2 * k)] / v[((2 * j) + 1)]
    PASS: v[((2 * j) + 1)] / v[(2 * k)]
    PASS: v[(k * 2)] / v[((2 * j) - 1)]
    PASS: v[((2 * j) - 1)] / v[(k * 2)]
    PASS: v[-((2 * k))] / v[((2 * j) + 1)]
    PASS: v[((2 * j) + 1)] / v[-((2 * k))]
    PASS: v[((2 * k) + 1)] / v[((2 * j) + 3)]
    PASS: v[((2 * j) + 3)] / v[((2 * k) + 1)]
    PASS: v[(2 * k)] / v[(2 * j)]
    PASS: v[(2 * j)] / v[(2 * k)]
    PASS: v[(k * j)] / v[2]
    PASS: v[2] / v[(k * j)]
    PASS: v[(k * j)] / v[3]
    PASS: v[3] / v[(k * j)]
    PASS: v[(k + 1)] / v[(2 * j)]
    PASS: v[(2 * j)] / v[(k + 1)]
    |}]

let%expect_test "Index overlap: Single / Upfrom" =
  check_may_idx_overlap "2" "3:" ~expected:false;
  check_may_idx_overlap "4" "3:" ~expected:true;
  check_may_idx_overlap "3" "3:" ~expected:true;
  check_may_idx_overlap "10" "10:" ~expected:true;
  check_may_idx_overlap "1" "10:" ~expected:false;
  check_may_idx_overlap "k" "3:" ~expected:true;
  check_may_idx_overlap "3" "k:" ~expected:true;
  check_may_idx_overlap "k" "j:" ~expected:true;
  [%expect
    {|
    PASS: v[2] / v[3:]
    PASS: v[3:] / v[2]
    PASS: v[4] / v[3:]
    PASS: v[3:] / v[4]
    PASS: v[3] / v[3:]
    PASS: v[3:] / v[3]
    PASS: v[10] / v[10:]
    PASS: v[10:] / v[10]
    PASS: v[1] / v[10:]
    PASS: v[10:] / v[1]
    PASS: v[k] / v[3:]
    PASS: v[3:] / v[k]
    PASS: v[3] / v[k:]
    PASS: v[k:] / v[3]
    PASS: v[k] / v[j:]
    PASS: v[j:] / v[k]
    |}]

let%expect_test "Index overlap: Single / Between" =
  check_may_idx_overlap "2" "1:2" ~expected:true;
  check_may_idx_overlap "3" "1:2" ~expected:false;
  check_may_idx_overlap "1" "1:k" ~expected:true;
  check_may_idx_overlap "2" "2:4" ~expected:true;
  check_may_idx_overlap "4" "2:4" ~expected:true;
  check_may_idx_overlap "1" "2:4" ~expected:false;
  check_may_idx_overlap "3" "3:3" ~expected:true;
  check_may_idx_overlap "3" "4:3" ~expected:false;
  check_may_idx_overlap "k" "2:4" ~expected:true;
  check_may_idx_overlap "1" "2:k" ~expected:false;
  check_may_idx_overlap "10" "k:9" ~expected:false;
  [%expect
    {|
    PASS: v[2] / v[1:2]
    PASS: v[1:2] / v[2]
    PASS: v[3] / v[1:2]
    PASS: v[1:2] / v[3]
    PASS: v[1] / v[1:k]
    PASS: v[1:k] / v[1]
    PASS: v[2] / v[2:4]
    PASS: v[2:4] / v[2]
    PASS: v[4] / v[2:4]
    PASS: v[2:4] / v[4]
    PASS: v[1] / v[2:4]
    PASS: v[2:4] / v[1]
    PASS: v[3] / v[3:3]
    PASS: v[3:3] / v[3]
    PASS: v[3] / v[4:3]
    PASS: v[4:3] / v[3]
    PASS: v[k] / v[2:4]
    PASS: v[2:4] / v[k]
    PASS: v[1] / v[2:k]
    PASS: v[2:k] / v[1]
    PASS: v[10] / v[k:9]
    PASS: v[k:9] / v[10]
    |}]

let%expect_test "Index overlap: Single / MultiIndex" =
  check_may_idx_overlap "2" "{1, 3}" ~expected:false;
  check_may_idx_overlap "3" "{1, 3}" ~expected:true;
  check_may_idx_overlap "2" "{1, k}" ~expected:true;
  check_may_idx_overlap "1" "{1}" ~expected:true;
  check_may_idx_overlap "2" "{1}" ~expected:false;
  check_may_idx_overlap "10" "{10, 1, 10}" ~expected:true;
  check_may_idx_overlap "10" "{1, 9}" ~expected:false;
  check_may_idx_overlap "k" "{1, 3}" ~expected:true;
  check_may_idx_overlap "2" "idx" ~expected:true;
  [%expect
    {|
    PASS: v[2] / v[{1, 3}]
    PASS: v[{1, 3}] / v[2]
    PASS: v[3] / v[{1, 3}]
    PASS: v[{1, 3}] / v[3]
    PASS: v[2] / v[{1, k}]
    PASS: v[{1, k}] / v[2]
    PASS: v[1] / v[{1}]
    PASS: v[{1}] / v[1]
    PASS: v[2] / v[{1}]
    PASS: v[{1}] / v[2]
    PASS: v[10] / v[{10, 1, 10}]
    PASS: v[{10, 1, 10}] / v[10]
    PASS: v[10] / v[{1, 9}]
    PASS: v[{1, 9}] / v[10]
    PASS: v[k] / v[{1, 3}]
    PASS: v[{1, 3}] / v[k]
    PASS: v[2] / v[idx]
    PASS: v[idx] / v[2]
    |}]

let%expect_test "Index overlap: Upfrom / All" =
  check_may_idx_overlap "3:" ":" ~expected:true;
  check_may_idx_overlap "1:" ":" ~expected:true;
  check_may_idx_overlap "10:" ":" ~expected:true;
  check_may_idx_overlap "k:" ":" ~expected:true;
  [%expect
    {|
    PASS: v[3:] / v[:]
    PASS: v[:] / v[3:]
    PASS: v[1:] / v[:]
    PASS: v[:] / v[1:]
    PASS: v[10:] / v[:]
    PASS: v[:] / v[10:]
    PASS: v[k:] / v[:]
    PASS: v[:] / v[k:]
    |}]

let%expect_test "Index overlap: Upfrom / Single" =
  check_may_idx_overlap "3:" "2" ~expected:false;
  check_may_idx_overlap "3:" "4" ~expected:true;
  check_may_idx_overlap "3:" "3" ~expected:true;
  check_may_idx_overlap "10:" "10" ~expected:true;
  check_may_idx_overlap "10:" "1" ~expected:false;
  check_may_idx_overlap "3:" "k" ~expected:true;
  check_may_idx_overlap "k:" "3" ~expected:true;
  check_may_idx_overlap "j:" "k" ~expected:true;
  [%expect
    {|
    PASS: v[3:] / v[2]
    PASS: v[2] / v[3:]
    PASS: v[3:] / v[4]
    PASS: v[4] / v[3:]
    PASS: v[3:] / v[3]
    PASS: v[3] / v[3:]
    PASS: v[10:] / v[10]
    PASS: v[10] / v[10:]
    PASS: v[10:] / v[1]
    PASS: v[1] / v[10:]
    PASS: v[3:] / v[k]
    PASS: v[k] / v[3:]
    PASS: v[k:] / v[3]
    PASS: v[3] / v[k:]
    PASS: v[j:] / v[k]
    PASS: v[k] / v[j:]
    |}]

let%expect_test "Index overlap: Upfrom / Upfrom" =
  check_may_idx_overlap "2:" "3:" ~expected:true;
  check_may_idx_overlap "1:" "1:" ~expected:true;
  check_may_idx_overlap "3:" "3:" ~expected:true;
  check_may_idx_overlap "1:" "10:" ~expected:true;
  check_may_idx_overlap "10:" "10:" ~expected:true;
  check_may_idx_overlap "k:" "j:" ~expected:true;
  [%expect
    {|
    PASS: v[2:] / v[3:]
    PASS: v[3:] / v[2:]
    PASS: v[1:] / v[1:]
    PASS: v[1:] / v[1:]
    PASS: v[3:] / v[3:]
    PASS: v[3:] / v[3:]
    PASS: v[1:] / v[10:]
    PASS: v[10:] / v[1:]
    PASS: v[10:] / v[10:]
    PASS: v[10:] / v[10:]
    PASS: v[k:] / v[j:]
    PASS: v[j:] / v[k:]
    |}]

let%expect_test "Index overlap: Upfrom / Between" =
  check_may_idx_overlap "3:" "1:2" ~expected:false;
  check_may_idx_overlap "3:" ":2" ~expected:false;
  check_may_idx_overlap "3:" "3:4" ~expected:true;
  check_may_idx_overlap "3:" "1:3" ~expected:true;
  check_may_idx_overlap "3:" "4:5" ~expected:true;
  check_may_idx_overlap "10:" "10:10" ~expected:true;
  check_may_idx_overlap "3:" "4:3" ~expected:false;
  check_may_idx_overlap "3:" "k:2" ~expected:false;
  check_may_idx_overlap "k:" "1:2" ~expected:true;
  check_may_idx_overlap "3:" "1:j" ~expected:true;
  [%expect
    {|
    PASS: v[3:] / v[1:2]
    PASS: v[1:2] / v[3:]
    PASS: v[3:] / v[1:2]
    PASS: v[1:2] / v[3:]
    PASS: v[3:] / v[3:4]
    PASS: v[3:4] / v[3:]
    PASS: v[3:] / v[1:3]
    PASS: v[1:3] / v[3:]
    PASS: v[3:] / v[4:5]
    PASS: v[4:5] / v[3:]
    PASS: v[10:] / v[10:10]
    PASS: v[10:10] / v[10:]
    PASS: v[3:] / v[4:3]
    PASS: v[4:3] / v[3:]
    PASS: v[3:] / v[k:2]
    PASS: v[k:2] / v[3:]
    PASS: v[k:] / v[1:2]
    PASS: v[1:2] / v[k:]
    PASS: v[3:] / v[1:j]
    PASS: v[1:j] / v[3:]
    |}]

let%expect_test "Index overlap: Upfrom / MultiIndex" =
  check_may_idx_overlap "3:" "{1, 3}" ~expected:true;
  check_may_idx_overlap "3:" "{1, 2}" ~expected:false;
  check_may_idx_overlap "3:" "{4, 5}" ~expected:true;
  check_may_idx_overlap "10:" "{10, 1, 10}" ~expected:true;
  check_may_idx_overlap "10:" "{1, 9}" ~expected:false;
  check_may_idx_overlap "k:" "{1, 3}" ~expected:true;
  check_may_idx_overlap "3:" "idx" ~expected:true;
  check_may_idx_overlap "3:" "{1, k}" ~expected:true;
  [%expect
    {|
    PASS: v[3:] / v[{1, 3}]
    PASS: v[{1, 3}] / v[3:]
    PASS: v[3:] / v[{1, 2}]
    PASS: v[{1, 2}] / v[3:]
    PASS: v[3:] / v[{4, 5}]
    PASS: v[{4, 5}] / v[3:]
    PASS: v[10:] / v[{10, 1, 10}]
    PASS: v[{10, 1, 10}] / v[10:]
    PASS: v[10:] / v[{1, 9}]
    PASS: v[{1, 9}] / v[10:]
    PASS: v[k:] / v[{1, 3}]
    PASS: v[{1, 3}] / v[k:]
    PASS: v[3:] / v[idx]
    PASS: v[idx] / v[3:]
    PASS: v[3:] / v[{1, k}]
    PASS: v[{1, k}] / v[3:]
    |}]

let%expect_test "Index overlap: Between / All" =
  check_may_idx_overlap "1:2" ":" ~expected:true;
  check_may_idx_overlap "3:2" ":" ~expected:false;
  check_may_idx_overlap "1:10" ":" ~expected:true;
  check_may_idx_overlap "5:5" ":" ~expected:true;
  check_may_idx_overlap "10:1" ":" ~expected:false;
  check_may_idx_overlap "k:j" ":" ~expected:true;
  [%expect
    {|
    PASS: v[1:2] / v[:]
    PASS: v[:] / v[1:2]
    PASS: v[3:2] / v[:]
    PASS: v[:] / v[3:2]
    PASS: v[1:10] / v[:]
    PASS: v[:] / v[1:10]
    PASS: v[5:5] / v[:]
    PASS: v[:] / v[5:5]
    PASS: v[10:1] / v[:]
    PASS: v[:] / v[10:1]
    PASS: v[k:j] / v[:]
    PASS: v[:] / v[k:j]
    |}]

let%expect_test "Index overlap: Between / Single" =
  check_may_idx_overlap "1:2" "2" ~expected:true;
  check_may_idx_overlap "1:2" "3" ~expected:false;
  check_may_idx_overlap "1:k" "1" ~expected:true;
  check_may_idx_overlap "2:4" "2" ~expected:true;
  check_may_idx_overlap "2:4" "4" ~expected:true;
  check_may_idx_overlap "2:4" "1" ~expected:false;
  check_may_idx_overlap "3:3" "3" ~expected:true;
  check_may_idx_overlap "4:3" "3" ~expected:false;
  check_may_idx_overlap "2:4" "k" ~expected:true;
  check_may_idx_overlap "2:k" "1" ~expected:false;
  check_may_idx_overlap "k:9" "10" ~expected:false;
  [%expect
    {|
    PASS: v[1:2] / v[2]
    PASS: v[2] / v[1:2]
    PASS: v[1:2] / v[3]
    PASS: v[3] / v[1:2]
    PASS: v[1:k] / v[1]
    PASS: v[1] / v[1:k]
    PASS: v[2:4] / v[2]
    PASS: v[2] / v[2:4]
    PASS: v[2:4] / v[4]
    PASS: v[4] / v[2:4]
    PASS: v[2:4] / v[1]
    PASS: v[1] / v[2:4]
    PASS: v[3:3] / v[3]
    PASS: v[3] / v[3:3]
    PASS: v[4:3] / v[3]
    PASS: v[3] / v[4:3]
    PASS: v[2:4] / v[k]
    PASS: v[k] / v[2:4]
    PASS: v[2:k] / v[1]
    PASS: v[1] / v[2:k]
    PASS: v[k:9] / v[10]
    PASS: v[10] / v[k:9]
    |}]

let%expect_test "Index overlap: Between / Upfrom" =
  check_may_idx_overlap "1:2" "3:" ~expected:false;
  check_may_idx_overlap ":2" "3:" ~expected:false;
  check_may_idx_overlap "3:4" "3:" ~expected:true;
  check_may_idx_overlap "1:3" "3:" ~expected:true;
  check_may_idx_overlap "4:5" "3:" ~expected:true;
  check_may_idx_overlap "10:10" "10:" ~expected:true;
  check_may_idx_overlap "4:3" "3:" ~expected:false;
  check_may_idx_overlap "k:2" "3:" ~expected:false;
  check_may_idx_overlap "1:2" "k:" ~expected:true;
  check_may_idx_overlap "1:j" "3:" ~expected:true;
  [%expect
    {|
    PASS: v[1:2] / v[3:]
    PASS: v[3:] / v[1:2]
    PASS: v[1:2] / v[3:]
    PASS: v[3:] / v[1:2]
    PASS: v[3:4] / v[3:]
    PASS: v[3:] / v[3:4]
    PASS: v[1:3] / v[3:]
    PASS: v[3:] / v[1:3]
    PASS: v[4:5] / v[3:]
    PASS: v[3:] / v[4:5]
    PASS: v[10:10] / v[10:]
    PASS: v[10:] / v[10:10]
    PASS: v[4:3] / v[3:]
    PASS: v[3:] / v[4:3]
    PASS: v[k:2] / v[3:]
    PASS: v[3:] / v[k:2]
    PASS: v[1:2] / v[k:]
    PASS: v[k:] / v[1:2]
    PASS: v[1:j] / v[3:]
    PASS: v[3:] / v[1:j]
    |}]

let%expect_test "Index overlap: Between / Between" =
  check_may_idx_overlap "1:2" "3:4" ~expected:false;
  check_may_idx_overlap "1:2" "2:4" ~expected:true;
  check_may_idx_overlap "1:k" "j:4" ~expected:true;
  check_may_idx_overlap "k:2" "3:j" ~expected:false;
  check_may_idx_overlap "3:2" "1:4" ~expected:false;
  check_may_idx_overlap "2:4" "2:4" ~expected:true;
  check_may_idx_overlap "1:10" "2:3" ~expected:true;
  check_may_idx_overlap "1:3" "3:5" ~expected:true;
  check_may_idx_overlap "5:5" "1:10" ~expected:true;
  check_may_idx_overlap "10:10" "10:10" ~expected:true;
  check_may_idx_overlap "3:2" "4:3" ~expected:false;
  check_may_idx_overlap "5:k" "j:4" ~expected:false;
  [%expect
    {|
    PASS: v[1:2] / v[3:4]
    PASS: v[3:4] / v[1:2]
    PASS: v[1:2] / v[2:4]
    PASS: v[2:4] / v[1:2]
    PASS: v[1:k] / v[j:4]
    PASS: v[j:4] / v[1:k]
    PASS: v[k:2] / v[3:j]
    PASS: v[3:j] / v[k:2]
    PASS: v[3:2] / v[1:4]
    PASS: v[1:4] / v[3:2]
    PASS: v[2:4] / v[2:4]
    PASS: v[2:4] / v[2:4]
    PASS: v[1:10] / v[2:3]
    PASS: v[2:3] / v[1:10]
    PASS: v[1:3] / v[3:5]
    PASS: v[3:5] / v[1:3]
    PASS: v[5:5] / v[1:10]
    PASS: v[1:10] / v[5:5]
    PASS: v[10:10] / v[10:10]
    PASS: v[10:10] / v[10:10]
    PASS: v[3:2] / v[4:3]
    PASS: v[4:3] / v[3:2]
    PASS: v[5:k] / v[j:4]
    PASS: v[j:4] / v[5:k]
    |}]

let%expect_test "Index overlap: Between / MultiIndex" =
  check_may_idx_overlap "2:2" "{1, 3}" ~expected:false;
  check_may_idx_overlap "2:3" "{1, 3}" ~expected:true;
  check_may_idx_overlap "1:2" "idx" ~expected:true;
  check_may_idx_overlap "2:4" "{2, 4}" ~expected:true;
  check_may_idx_overlap "2:4" "{3}" ~expected:true;
  check_may_idx_overlap "2:4" "{1, 5}" ~expected:false;
  check_may_idx_overlap "2:4" "{4, 2, 4}" ~expected:true;
  check_may_idx_overlap "3:2" "{2, 3}" ~expected:false;
  check_may_idx_overlap "2:4" "{k}" ~expected:true;
  check_may_idx_overlap "k:2" "{3, 4}" ~expected:false;
  check_may_idx_overlap "3:j" "{1, 2}" ~expected:false;
  [%expect
    {|
    PASS: v[2:2] / v[{1, 3}]
    PASS: v[{1, 3}] / v[2:2]
    PASS: v[2:3] / v[{1, 3}]
    PASS: v[{1, 3}] / v[2:3]
    PASS: v[1:2] / v[idx]
    PASS: v[idx] / v[1:2]
    PASS: v[2:4] / v[{2, 4}]
    PASS: v[{2, 4}] / v[2:4]
    PASS: v[2:4] / v[{3}]
    PASS: v[{3}] / v[2:4]
    PASS: v[2:4] / v[{1, 5}]
    PASS: v[{1, 5}] / v[2:4]
    PASS: v[2:4] / v[{4, 2, 4}]
    PASS: v[{4, 2, 4}] / v[2:4]
    PASS: v[3:2] / v[{2, 3}]
    PASS: v[{2, 3}] / v[3:2]
    PASS: v[2:4] / v[{k}]
    PASS: v[{k}] / v[2:4]
    PASS: v[k:2] / v[{3, 4}]
    PASS: v[{3, 4}] / v[k:2]
    PASS: v[3:j] / v[{1, 2}]
    PASS: v[{1, 2}] / v[3:j]
    |}]

let%expect_test "Index overlap: MultiIndex / All" =
  check_may_idx_overlap "{1, 3}" ":" ~expected:true;
  check_may_idx_overlap "{1}" ":" ~expected:true;
  check_may_idx_overlap "{10, 1, 10}" ":" ~expected:true;
  check_may_idx_overlap "idx" ":" ~expected:true;
  check_may_idx_overlap "{k, j}" ":" ~expected:true;
  [%expect
    {|
    PASS: v[{1, 3}] / v[:]
    PASS: v[:] / v[{1, 3}]
    PASS: v[{1}] / v[:]
    PASS: v[:] / v[{1}]
    PASS: v[{10, 1, 10}] / v[:]
    PASS: v[:] / v[{10, 1, 10}]
    PASS: v[idx] / v[:]
    PASS: v[:] / v[idx]
    PASS: v[{k, j}] / v[:]
    PASS: v[:] / v[{k, j}]
    |}]

let%expect_test "Index overlap: MultiIndex / Single" =
  check_may_idx_overlap "{1, 3}" "2" ~expected:false;
  check_may_idx_overlap "{1, 3}" "3" ~expected:true;
  check_may_idx_overlap "{1, k}" "2" ~expected:true;
  check_may_idx_overlap "{1}" "1" ~expected:true;
  check_may_idx_overlap "{1}" "2" ~expected:false;
  check_may_idx_overlap "{10, 1, 10}" "10" ~expected:true;
  check_may_idx_overlap "{1, 9}" "10" ~expected:false;
  check_may_idx_overlap "{1, 3}" "k" ~expected:true;
  check_may_idx_overlap "idx" "2" ~expected:true;
  [%expect
    {|
    PASS: v[{1, 3}] / v[2]
    PASS: v[2] / v[{1, 3}]
    PASS: v[{1, 3}] / v[3]
    PASS: v[3] / v[{1, 3}]
    PASS: v[{1, k}] / v[2]
    PASS: v[2] / v[{1, k}]
    PASS: v[{1}] / v[1]
    PASS: v[1] / v[{1}]
    PASS: v[{1}] / v[2]
    PASS: v[2] / v[{1}]
    PASS: v[{10, 1, 10}] / v[10]
    PASS: v[10] / v[{10, 1, 10}]
    PASS: v[{1, 9}] / v[10]
    PASS: v[10] / v[{1, 9}]
    PASS: v[{1, 3}] / v[k]
    PASS: v[k] / v[{1, 3}]
    PASS: v[idx] / v[2]
    PASS: v[2] / v[idx]
    |}]

let%expect_test "Index overlap: MultiIndex / Upfrom" =
  check_may_idx_overlap "{1, 3}" "3:" ~expected:true;
  check_may_idx_overlap "{1, 2}" "3:" ~expected:false;
  check_may_idx_overlap "{4, 5}" "3:" ~expected:true;
  check_may_idx_overlap "{10, 1, 10}" "10:" ~expected:true;
  check_may_idx_overlap "{1, 9}" "10:" ~expected:false;
  check_may_idx_overlap "{1, 3}" "k:" ~expected:true;
  check_may_idx_overlap "idx" "3:" ~expected:true;
  check_may_idx_overlap "{1, k}" "3:" ~expected:true;
  [%expect
    {|
    PASS: v[{1, 3}] / v[3:]
    PASS: v[3:] / v[{1, 3}]
    PASS: v[{1, 2}] / v[3:]
    PASS: v[3:] / v[{1, 2}]
    PASS: v[{4, 5}] / v[3:]
    PASS: v[3:] / v[{4, 5}]
    PASS: v[{10, 1, 10}] / v[10:]
    PASS: v[10:] / v[{10, 1, 10}]
    PASS: v[{1, 9}] / v[10:]
    PASS: v[10:] / v[{1, 9}]
    PASS: v[{1, 3}] / v[k:]
    PASS: v[k:] / v[{1, 3}]
    PASS: v[idx] / v[3:]
    PASS: v[3:] / v[idx]
    PASS: v[{1, k}] / v[3:]
    PASS: v[3:] / v[{1, k}]
    |}]

let%expect_test "Index overlap: MultiIndex / Between" =
  check_may_idx_overlap "{1, 3}" "2:2" ~expected:false;
  check_may_idx_overlap "{1, 3}" "2:3" ~expected:true;
  check_may_idx_overlap "idx" "1:2" ~expected:true;
  check_may_idx_overlap "{2, 4}" "2:4" ~expected:true;
  check_may_idx_overlap "{3}" "2:4" ~expected:true;
  check_may_idx_overlap "{1, 5}" "2:4" ~expected:false;
  check_may_idx_overlap "{4, 2, 4}" "2:4" ~expected:true;
  check_may_idx_overlap "{2, 3}" "3:2" ~expected:false;
  check_may_idx_overlap "{k}" "2:4" ~expected:true;
  check_may_idx_overlap "{3, 4}" "k:2" ~expected:false;
  check_may_idx_overlap "{1, 2}" "3:j" ~expected:false;
  [%expect
    {|
    PASS: v[{1, 3}] / v[2:2]
    PASS: v[2:2] / v[{1, 3}]
    PASS: v[{1, 3}] / v[2:3]
    PASS: v[2:3] / v[{1, 3}]
    PASS: v[idx] / v[1:2]
    PASS: v[1:2] / v[idx]
    PASS: v[{2, 4}] / v[2:4]
    PASS: v[2:4] / v[{2, 4}]
    PASS: v[{3}] / v[2:4]
    PASS: v[2:4] / v[{3}]
    PASS: v[{1, 5}] / v[2:4]
    PASS: v[2:4] / v[{1, 5}]
    PASS: v[{4, 2, 4}] / v[2:4]
    PASS: v[2:4] / v[{4, 2, 4}]
    PASS: v[{2, 3}] / v[3:2]
    PASS: v[3:2] / v[{2, 3}]
    PASS: v[{k}] / v[2:4]
    PASS: v[2:4] / v[{k}]
    PASS: v[{3, 4}] / v[k:2]
    PASS: v[k:2] / v[{3, 4}]
    PASS: v[{1, 2}] / v[3:j]
    PASS: v[3:j] / v[{1, 2}]
    |}]

let%expect_test "Index overlap: MultiIndex / MultiIndex" =
  check_may_idx_overlap "{1, 3}" "{2, 4}" ~expected:false;
  check_may_idx_overlap "{1, 3}" "{3, 4}" ~expected:true;
  check_may_idx_overlap "{1, 1}" "{2, 2}" ~expected:false;
  check_may_idx_overlap "idx" "{1, 3}" ~expected:true;
  check_may_idx_overlap "{1, 3}" "{1, 3}" ~expected:true;
  check_may_idx_overlap "{3, 1}" "{1, 3}" ~expected:true;
  check_may_idx_overlap "{1, 1}" "{1, 1}" ~expected:true;
  check_may_idx_overlap "{1}" "{2}" ~expected:false;
  check_may_idx_overlap "{1}" "{1}" ~expected:true;
  check_may_idx_overlap "idx" "idx" ~expected:true;
  check_may_idx_overlap "{k}" "{j}" ~expected:true;
  check_may_idx_overlap "{1, k}" "{2, 3}" ~expected:true;
  [%expect
    {|
    PASS: v[{1, 3}] / v[{2, 4}]
    PASS: v[{2, 4}] / v[{1, 3}]
    PASS: v[{1, 3}] / v[{3, 4}]
    PASS: v[{3, 4}] / v[{1, 3}]
    PASS: v[{1, 1}] / v[{2, 2}]
    PASS: v[{2, 2}] / v[{1, 1}]
    PASS: v[idx] / v[{1, 3}]
    PASS: v[{1, 3}] / v[idx]
    PASS: v[{1, 3}] / v[{1, 3}]
    PASS: v[{1, 3}] / v[{1, 3}]
    PASS: v[{3, 1}] / v[{1, 3}]
    PASS: v[{1, 3}] / v[{3, 1}]
    PASS: v[{1, 1}] / v[{1, 1}]
    PASS: v[{1, 1}] / v[{1, 1}]
    PASS: v[{1}] / v[{2}]
    PASS: v[{2}] / v[{1}]
    PASS: v[{1}] / v[{1}]
    PASS: v[{1}] / v[{1}]
    PASS: v[idx] / v[idx]
    PASS: v[idx] / v[idx]
    PASS: v[{k}] / v[{j}]
    PASS: v[{j}] / v[{k}]
    PASS: v[{1, k}] / v[{2, 3}]
    PASS: v[{2, 3}] / v[{1, k}]
    |}]

let%expect_test "Different columns remain disjoint after All" =
  check_read_overlap
    {|model { matrix[4, 4] v; print(v[:, 1]); print(v[:, 2]); }|} false;
  [%expect {|
    PASS: v[:, 1] / v[:, 2]
    PASS: v[:, 2] / v[:, 1]
    |}]

let%expect_test "Overlapping row ranges share a column" =
  check_read_overlap
    {|model { matrix[4, 4] v; print(v[1:2, 1]); print(v[2:3, 1]); }|} true;
  [%expect
    {|
    PASS: v[1:2, 1] / v[2:3, 1]
    PASS: v[2:3, 1] / v[1:2, 1]
    |}]

let%expect_test "Different columns separate unknown row selections" =
  check_read_overlap
    {|data { int k; array[2] int idx; }
      model { matrix[4, 4] v; print(v[idx, 1]); print(v[k:4, 2]); }|}
    false;
  [%expect
    {|
    PASS: v[idx, 1] / v[k:4, 2]
    PASS: v[k:4, 2] / v[idx, 1]
    |}]

let%expect_test "Disjoint column ranges separate matching row arrays" =
  check_read_overlap
    {|model { matrix[4, 4] v; print(v[{1, 3}, 1:2]); print(v[{1, 3}, 3:4]); }|}
    false;
  [%expect
    {|
    PASS: v[{1, 3}, 1:2] / v[{1, 3}, 3:4]
    PASS: v[{1, 3}, 3:4] / v[{1, 3}, 1:2]
    |}]

let%expect_test "A whole column overlaps a whole row at their intersection" =
  check_read_overlap
    {|model { matrix[4, 4] v; print(v[:, 1]); print(v[1, :]); }|} true;
  [%expect {|
    PASS: v[:, 1] / v[1, :]
    PASS: v[1, :] / v[:, 1]
    |}]

let%expect_test "Nested slices retain coordinates of the original variable" =
  check_read_overlap {|model { vector[4] v; print(v[2:4][1]); print(v[2]); }|}
    true;
  check_read_overlap {|model { vector[4] v; print(v[2:4][1]); print(v[1]); }|}
    false;
  [%expect
    {|
    PASS: v[2:4] / v[2]
    PASS: v[2] / v[2:4]
    PASS: v[2:4] / v[1]
    PASS: v[1] / v[2:4]
    |}]

let%expect_test "Empty selections never overlap any index form" =
  let empty_array =
    Expr.Helpers.internal_funapp FnMakeArray []
      {Expr.Typed.Meta.empty with type_= UArray UInt} in
  let unknown_array = Expr.Helpers.variable "idx" in
  let candidates =
    [ Index.All; Single (Expr.Helpers.int 2); Upfrom (Expr.Helpers.int 2)
    ; Between (Expr.Helpers.int 1, Expr.Helpers.int 3)
    ; MultiIndex (Expr.Helpers.array_expr [Expr.Helpers.int 2])
    ; MultiIndex unknown_array; MultiIndex empty_array ] in
  List.iter
    [ Index.MultiIndex empty_array
    ; Between (Expr.Helpers.int 3, Expr.Helpers.int 2) ]
    ~f:(fun empty_index ->
      List.iter candidates ~f:(fun other_index ->
          check_overlap [empty_index] [other_index] false;
          check_overlap [other_index] [empty_index] false));
  [%expect
    {|
    PASS: v[{}] / v[:]
    PASS: v[:] / v[{}]
    PASS: v[{}] / v[2]
    PASS: v[2] / v[{}]
    PASS: v[{}] / v[2:]
    PASS: v[2:] / v[{}]
    PASS: v[{}] / v[1:3]
    PASS: v[1:3] / v[{}]
    PASS: v[{}] / v[{2}]
    PASS: v[{2}] / v[{}]
    PASS: v[{}] / v[idx]
    PASS: v[idx] / v[{}]
    PASS: v[{}] / v[{}]
    PASS: v[{}] / v[{}]
    PASS: v[3:2] / v[:]
    PASS: v[:] / v[3:2]
    PASS: v[3:2] / v[2]
    PASS: v[2] / v[3:2]
    PASS: v[3:2] / v[2:]
    PASS: v[2:] / v[3:2]
    PASS: v[3:2] / v[1:3]
    PASS: v[1:3] / v[3:2]
    PASS: v[3:2] / v[{2}]
    PASS: v[{2}] / v[3:2]
    PASS: v[3:2] / v[idx]
    PASS: v[idx] / v[3:2]
    PASS: v[3:2] / v[{}]
    PASS: v[{}] / v[3:2]
    |}]
