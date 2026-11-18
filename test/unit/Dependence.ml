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

let print_assignment_accesses source =
  assignment_accesses source
  |> List.iter ~f:(fun statement_accesses ->
      print_locations "read paths" statement_accesses.Access.reads;
      print_locations "write paths" statement_accesses.writes)

let%expect_test
    "Access collection: index expressions contribute their own reads" =
  print_assignment_accesses
    {|data { array[3] int<lower=1, upper=2> idx; vector[4] w; }
      model { vector[4] v; v[idx[1]] = w[idx[2] * idx[3]]; }|};
  [%expect
    {|
    read paths: idx[1], idx[2], idx[3], w[(idx[2] * idx[3])]
    write paths: v[idx[1]]
    |}]

let%expect_test "Access collection: tuple fields precede element indices" =
  (* Tuple field numbers appear as Single indices in the access paths. *)
  print_assignment_accesses
    {|data { array[1] int<lower=1, upper=4> idx; vector[4] v; }
      model { tuple(vector[4], real) t; t.1[idx[1]] = v[2]; }|};
  [%expect {|
    read paths: idx[1], v[2]
    write paths: t[1][idx[1]]
    |}]

let%expect_test "Access collection: tuple fields have separate paths" =
  (* Tuple field numbers appear as Single indices in the access paths. *)
  print_assignment_accesses
    {|data { tuple(real, real) source; }
      model { tuple(real, real) t; t.1 = source.2; }|};
  [%expect {|
    read paths: source[2]
    write paths: t[1]
    |}]

let%expect_test
    "Access collection: tuple element reads keep the field and index" =
  (* The first path index identifies the tuple field. *)
  print_assignment_accesses
    {|data { tuple(vector[4], real) t; }
      model { real x = t.1[2]; }|};
  [%expect {|
    read paths: t[1][2]
    write paths: x
    |}]

let%expect_test "Access collection: slices retain their bounds" =
  print_assignment_accesses
    {|data { vector[4] w; }
      model { vector[4] v; v[1:2] = w[2:3]; }|};
  [%expect {|
    read paths: w[2:3]
    write paths: v[1:2]
    |}]

let%expect_test "Access collection: compound assignment reads its destination" =
  print_assignment_accesses
    {|parameters { vector[4] initial; }
      model { vector[4] v = initial; v[1] += 1; }|};
  [%expect
    {|
    read paths: initial
    write paths: v
    read paths: v[1]
    write paths: v[1]
    |}]

let%expect_test "Access collection: nested slices retain the base selection" =
  print_assignment_accesses
    {|data { vector[4] w; }
      model { real z = w[2:3][1]; }|};
  [%expect {|
    read paths: w[2:3]
    write paths: z
    |}]

let check_overlap first_path second_path expected =
  let actual = Access.may_overlap first_path second_path in
  if actual <> expected then
    Fmt.pr "FAIL: v%a / v%a: expected: %b result: %b@."
      (Index.pp_indices pp_index_expr)
      first_path
      (Index.pp_indices pp_index_expr)
      second_path expected actual
  else
    Fmt.pr "PASS: v%a / v%a; may overlap: %b@."
      (Index.pp_indices pp_index_expr)
      first_path
      (Index.pp_indices pp_index_expr)
      second_path actual

let pp_statement ppf pattern =
  let without_promotions =
    Expr.rewrite_bottom_up ~f:(fun expr ->
        match expr.Expr.pattern with
        | Promotion (original, _, _) -> original
        | _ -> expr) in
  Stmt.Located.pp ppf
    Stmt.
      { pattern=
          Pattern.map without_promotions
            (fun _ -> {pattern= Skip; meta= Location_span.empty})
            pattern
      ; meta= Location_span.empty }

let print_write_comparison variables source =
  let statements = statement_map source |> LabelMap.bindings in
  let written_vars =
    List.fold_left statements ~init:Set.Poly.empty
      ~f:(fun names (_, (pattern, _)) ->
        List.fold_left (Access.collect_accesses pattern).writes ~init:names
          ~f:(fun collected write ->
            Set.Poly.add write.Access.variable collected)) in
  let assignments =
    List.filter_map statements ~f:(fun (_, (pattern, _)) ->
        match pattern with
        | Stmt.Pattern.Assignment _ -> (
            match (Access.collect_accesses pattern).writes with
            | [write] when List.mem write.variable ~set:variables ->
                Some (pattern, write)
            | _ -> None)
        | _ -> None) in
  match assignments with
  | [(previous_statement, previous); (next_statement, next_write)] ->
      Fmt.pr "previous: %a@.next: %a@." pp_statement previous_statement
        pp_statement next_statement;
      Fmt.pr "may overlap: %b@.previous overwritten: %b@."
        (Access.Location.same_var previous next_write
        && Access.may_overlap previous.path next_write.path)
        (Access.is_overwritten written_vars previous next_write)
  | _ -> failwith "Expected two writes to the selected variables"

let%expect_test "Overwrite: different literal elements retain the earlier write"
    =
  print_write_comparison ["v"] {|model { vector[4] v; v[1] = 1; v[2] = 2; }|};
  [%expect
    {|
    previous: v[1] = 1;
    next: v[2] = 2;
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: the same literal element replaces the earlier write"
    =
  print_write_comparison ["v"] {|model { vector[4] v; v[1] = 1; v[1] = 2; }|};
  [%expect
    {|
    previous: v[1] = 1;
    next: v[1] = 2;
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: the same stable index replaces the earlier write" =
  print_write_comparison ["v"]
    {|data { int<lower=1, upper=4> k; }
      model { vector[4] v; v[k] = 1; v[k] = 2; }|};
  [%expect
    {|
    previous: v[k] = 1;
    next: v[k] = 2;
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: different symbolic indices can alias" =
  print_write_comparison ["v"]
    {|data { int<lower=1, upper=4> k; int<lower=1, upper=4> j; }
      model { vector[4] v; v[k] = 1; v[j] = 2; }|};
  [%expect
    {|
    previous: v[k] = 1;
    next: v[j] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: algebraic equivalence is not yet used" =
  print_write_comparison ["v"]
    {|data { int<lower=1, upper=2> k; }
      model { vector[4] v; v[k + k] = 1; v[2 * k] = 2; }|};
  [%expect
    {|
    previous: v[(k + k)] = 1;
    next: v[(2 * k)] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: stable indirect indices replace the earlier write" =
  print_write_comparison ["v"]
    {|data { int<lower=1, upper=4> k; array[4] int<lower=1, upper=4> idx; }
      model { vector[4] v; v[idx[k]] = 1; v[idx[k]] = 2; }|};
  [%expect
    {|
    previous: v[idx[k]] = 1;
    next: v[idx[k]] = 2;
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: nonlinear indices can alias a symbolic element" =
  print_write_comparison ["v"]
    {|data { int<lower=1, upper=2> k; int<lower=1, upper=4> j; }
      model { vector[4] v; v[k * k] = 1; v[j] = 2; }|};
  [%expect
    {|
    previous: v[(k * k)] = 1;
    next: v[j] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: changing the scalar index retains the earlier write"
    =
  print_write_comparison ["v"]
    {|model { vector[4] v; int k = 1; v[k] = 1; k = 2; v[k] = 2; }|};
  [%expect
    {|
    previous: v[k] = 1;
    next: v[k] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: changing the index array retains the earlier write"
    =
  print_write_comparison ["v"]
    {|model { vector[4] v; array[1] int idx = {1}; v[idx[1]] = 1; idx[1] = 2; v[idx[1]] = 2; }|};
  [%expect
    {|
    previous: v[idx[1]] = 1;
    next: v[idx[1]] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: different tuple fields remain distinct" =
  print_write_comparison ["t"]
    {|model { tuple(real, real) t; t.1 = 1; t.2 = 2; }|};
  [%expect
    {|
    previous: t.1 = 1;
    next: t.2 = 2;
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: an element write does not replace the whole vector"
    =
  print_write_comparison ["v"]
    {|model { vector[4] v; v = rep_vector(1, 4); v[1] = 2; }|};
  [%expect
    {|
    previous: v = rep_vector(1, 4);
    next: v[1] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: a whole-vector write replaces an element" =
  print_write_comparison ["v"]
    {|model { vector[4] v; v[1] = 1; v = rep_vector(2, 4); }|};
  [%expect
    {|
    previous: v[1] = 1;
    next: v = rep_vector(2, 4);
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: different variables remain distinct" =
  print_write_comparison ["v"; "w"]
    {|model { vector[4] v; vector[4] w; v[1] = 1; w = rep_vector(2, 4); }|};
  [%expect
    {|
    previous: v[1] = 1;
    next: w = rep_vector(2, 4);
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: whole writes to different variables remain distinct"
    =
  print_write_comparison ["v"; "w"]
    {|model { vector[4] v; vector[4] w; v = rep_vector(1, 4); w = rep_vector(2, 4); }|};
  [%expect
    {|
    previous: v = rep_vector(1, 4);
    next: w = rep_vector(2, 4);
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: disjoint slices retain the earlier write" =
  print_write_comparison ["v"]
    {|model { vector[4] v; v[1:2] = rep_vector(1, 2); v[3:4] = rep_vector(2, 2); }|};
  [%expect
    {|
    previous: v[1:2] = rep_vector(1, 2);
    next: v[3:4] = rep_vector(2, 2);
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test
    "Overwrite: tuple fields in different array elements remain distinct" =
  print_write_comparison ["t"]
    {|model { array[2] tuple(real, real) t; t[1].2 = 1; t[2].1 = 2; }|};
  [%expect
    {|
    previous: t[1].2 = 1;
    next: t[2].1 = 2;
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test
    "Overwrite: tuple fields in the same array element remain distinct" =
  print_write_comparison ["t"]
    {|model { array[2] tuple(real, real) t; t[1].1 = 1; t[1].2 = 2; }|};
  [%expect
    {|
    previous: t[1].1 = 1;
    next: t[1].2 = 2;
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: the same tuple field replaces the earlier write" =
  print_write_comparison ["t"]
    {|model { array[2] tuple(real, real) t; t[1].2 = 1; t[1].2 = 2; }|};
  [%expect
    {|
    previous: t[1].2 = 1;
    next: t[1].2 = 2;
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test
    "Overwrite: an element does not replace its enclosing tuple field" =
  print_write_comparison ["t"]
    {|model { tuple(vector[4], real) t; t.1 = rep_vector(1, 4); t.1[2] = 2; }|};
  [%expect
    {|
    previous: t.1 = rep_vector(1, 4);
    next: t.1[2] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: a tuple field replaces its contained element" =
  print_write_comparison ["t"]
    {|model { tuple(vector[4], real) t; t.1[2] = 1; t.1 = rep_vector(2, 4); }|};
  [%expect
    {|
    previous: t.1[2] = 1;
    next: t.1 = rep_vector(2, 4);
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: repeated RNG calls can select different elements" =
  print_write_comparison ["v"]
    {|functions { void f_rng(vector theta) {
        vector[num_elements(theta)] v;
        v[categorical_rng(theta)] = 1;
        v[categorical_rng(theta)] = 2;
      }} model {}|};
  [%expect
    {|
    previous: v[categorical_rng(theta)] = 1;
    next: v[categorical_rng(theta)] = 2;
    may overlap: true
    previous overwritten: false
    |}]

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

(* False proves disjointness; true means overlap cannot be ruled out. *)
let check_may_idx_overlap first_index second_index ~expected =
  let source =
    Fmt.str
      "data { int k; int j; array[10] int idx; } model { vector[10] v; \
       print(v[%s]); print(v[%s]); }"
      first_index second_index in
  check_read_overlap source expected

let%expect_test "Index overlap: All / All" =
  check_may_idx_overlap ":" ":" ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[:]; may overlap: true
    PASS: v[:] / v[:]; may overlap: true
    |}]

let%expect_test "Index overlap: All / Single" =
  check_may_idx_overlap ":" "2" ~expected:true;
  check_may_idx_overlap ":" "1" ~expected:true;
  check_may_idx_overlap ":" "10" ~expected:true;
  check_may_idx_overlap ":" "k" ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[2]; may overlap: true
    PASS: v[2] / v[:]; may overlap: true
    PASS: v[:] / v[1]; may overlap: true
    PASS: v[1] / v[:]; may overlap: true
    PASS: v[:] / v[10]; may overlap: true
    PASS: v[10] / v[:]; may overlap: true
    PASS: v[:] / v[k]; may overlap: true
    PASS: v[k] / v[:]; may overlap: true
    |}]

let%expect_test "Index overlap: All / Upfrom" =
  check_may_idx_overlap ":" "3:" ~expected:true;
  check_may_idx_overlap ":" "1:" ~expected:true;
  check_may_idx_overlap ":" "10:" ~expected:true;
  check_may_idx_overlap ":" "k:" ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[3:]; may overlap: true
    PASS: v[3:] / v[:]; may overlap: true
    PASS: v[:] / v[1:]; may overlap: true
    PASS: v[1:] / v[:]; may overlap: true
    PASS: v[:] / v[10:]; may overlap: true
    PASS: v[10:] / v[:]; may overlap: true
    PASS: v[:] / v[k:]; may overlap: true
    PASS: v[k:] / v[:]; may overlap: true
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
    PASS: v[:] / v[1:2]; may overlap: true
    PASS: v[1:2] / v[:]; may overlap: true
    PASS: v[:] / v[3:2]; may overlap: false
    PASS: v[3:2] / v[:]; may overlap: false
    PASS: v[:] / v[1:10]; may overlap: true
    PASS: v[1:10] / v[:]; may overlap: true
    PASS: v[:] / v[5:5]; may overlap: true
    PASS: v[5:5] / v[:]; may overlap: true
    PASS: v[:] / v[10:1]; may overlap: false
    PASS: v[10:1] / v[:]; may overlap: false
    PASS: v[:] / v[k:j]; may overlap: true
    PASS: v[k:j] / v[:]; may overlap: true
    |}]

let%expect_test "Index overlap: All / MultiIndex" =
  check_may_idx_overlap ":" "{1, 3}" ~expected:true;
  check_may_idx_overlap ":" "{1}" ~expected:true;
  check_may_idx_overlap ":" "{10, 1, 10}" ~expected:true;
  check_may_idx_overlap ":" "idx" ~expected:true;
  check_may_idx_overlap ":" "{k, j}" ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[:]; may overlap: true
    PASS: v[:] / v[{1}]; may overlap: true
    PASS: v[{1}] / v[:]; may overlap: true
    PASS: v[:] / v[{10, 1, 10}]; may overlap: true
    PASS: v[{10, 1, 10}] / v[:]; may overlap: true
    PASS: v[:] / v[idx]; may overlap: true
    PASS: v[idx] / v[:]; may overlap: true
    PASS: v[:] / v[{k, j}]; may overlap: true
    PASS: v[{k, j}] / v[:]; may overlap: true
    |}]

let%expect_test "Index overlap: Single / All" =
  check_may_idx_overlap "2" ":" ~expected:true;
  check_may_idx_overlap "1" ":" ~expected:true;
  check_may_idx_overlap "10" ":" ~expected:true;
  check_may_idx_overlap "k" ":" ~expected:true;
  [%expect
    {|
    PASS: v[2] / v[:]; may overlap: true
    PASS: v[:] / v[2]; may overlap: true
    PASS: v[1] / v[:]; may overlap: true
    PASS: v[:] / v[1]; may overlap: true
    PASS: v[10] / v[:]; may overlap: true
    PASS: v[:] / v[10]; may overlap: true
    PASS: v[k] / v[:]; may overlap: true
    PASS: v[:] / v[k]; may overlap: true
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
    PASS: v[1] / v[1]; may overlap: true
    PASS: v[1] / v[1]; may overlap: true
    PASS: v[1] / v[2]; may overlap: false
    PASS: v[2] / v[1]; may overlap: false
    PASS: v[(2 * k)] / v[(2 * k)]; may overlap: true
    PASS: v[(2 * k)] / v[(2 * k)]; may overlap: true
    PASS: v[k] / v[j]; may overlap: true
    PASS: v[j] / v[k]; may overlap: true
    PASS: v[(2 * k)] / v[((2 * k) + 1)]; may overlap: false
    PASS: v[((2 * k) + 1)] / v[(2 * k)]; may overlap: false
    PASS: v[10] / v[10]; may overlap: true
    PASS: v[10] / v[10]; may overlap: true
    PASS: v[1] / v[10]; may overlap: false
    PASS: v[10] / v[1]; may overlap: false
    PASS: v[1] / v[k]; may overlap: true
    PASS: v[k] / v[1]; may overlap: true
    PASS: v[k] / v[k]; may overlap: true
    PASS: v[k] / v[k]; may overlap: true
    PASS: v[(k + (1 * j))] / v[(k + (1 * j))]; may overlap: true
    PASS: v[(k + (1 * j))] / v[(k + (1 * j))]; may overlap: true
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
    PASS: v[(2 * k)] / v[((2 * j) + 1)]; may overlap: false
    PASS: v[((2 * j) + 1)] / v[(2 * k)]; may overlap: false
    PASS: v[(k * 2)] / v[((2 * j) - 1)]; may overlap: false
    PASS: v[((2 * j) - 1)] / v[(k * 2)]; may overlap: false
    PASS: v[-((2 * k))] / v[((2 * j) + 1)]; may overlap: false
    PASS: v[((2 * j) + 1)] / v[-((2 * k))]; may overlap: false
    PASS: v[((2 * k) + 1)] / v[((2 * j) + 3)]; may overlap: true
    PASS: v[((2 * j) + 3)] / v[((2 * k) + 1)]; may overlap: true
    PASS: v[(2 * k)] / v[(2 * j)]; may overlap: true
    PASS: v[(2 * j)] / v[(2 * k)]; may overlap: true
    PASS: v[(k * j)] / v[2]; may overlap: true
    PASS: v[2] / v[(k * j)]; may overlap: true
    PASS: v[(k * j)] / v[3]; may overlap: true
    PASS: v[3] / v[(k * j)]; may overlap: true
    PASS: v[(k + 1)] / v[(2 * j)]; may overlap: true
    PASS: v[(2 * j)] / v[(k + 1)]; may overlap: true
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
    PASS: v[2] / v[3:]; may overlap: false
    PASS: v[3:] / v[2]; may overlap: false
    PASS: v[4] / v[3:]; may overlap: true
    PASS: v[3:] / v[4]; may overlap: true
    PASS: v[3] / v[3:]; may overlap: true
    PASS: v[3:] / v[3]; may overlap: true
    PASS: v[10] / v[10:]; may overlap: true
    PASS: v[10:] / v[10]; may overlap: true
    PASS: v[1] / v[10:]; may overlap: false
    PASS: v[10:] / v[1]; may overlap: false
    PASS: v[k] / v[3:]; may overlap: true
    PASS: v[3:] / v[k]; may overlap: true
    PASS: v[3] / v[k:]; may overlap: true
    PASS: v[k:] / v[3]; may overlap: true
    PASS: v[k] / v[j:]; may overlap: true
    PASS: v[j:] / v[k]; may overlap: true
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
    PASS: v[2] / v[1:2]; may overlap: true
    PASS: v[1:2] / v[2]; may overlap: true
    PASS: v[3] / v[1:2]; may overlap: false
    PASS: v[1:2] / v[3]; may overlap: false
    PASS: v[1] / v[1:k]; may overlap: true
    PASS: v[1:k] / v[1]; may overlap: true
    PASS: v[2] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[2]; may overlap: true
    PASS: v[4] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[4]; may overlap: true
    PASS: v[1] / v[2:4]; may overlap: false
    PASS: v[2:4] / v[1]; may overlap: false
    PASS: v[3] / v[3:3]; may overlap: true
    PASS: v[3:3] / v[3]; may overlap: true
    PASS: v[3] / v[4:3]; may overlap: false
    PASS: v[4:3] / v[3]; may overlap: false
    PASS: v[k] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[k]; may overlap: true
    PASS: v[1] / v[2:k]; may overlap: false
    PASS: v[2:k] / v[1]; may overlap: false
    PASS: v[10] / v[k:9]; may overlap: false
    PASS: v[k:9] / v[10]; may overlap: false
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
    PASS: v[2] / v[{1, 3}]; may overlap: false
    PASS: v[{1, 3}] / v[2]; may overlap: false
    PASS: v[3] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[3]; may overlap: true
    PASS: v[2] / v[{1, k}]; may overlap: true
    PASS: v[{1, k}] / v[2]; may overlap: true
    PASS: v[1] / v[{1}]; may overlap: true
    PASS: v[{1}] / v[1]; may overlap: true
    PASS: v[2] / v[{1}]; may overlap: false
    PASS: v[{1}] / v[2]; may overlap: false
    PASS: v[10] / v[{10, 1, 10}]; may overlap: true
    PASS: v[{10, 1, 10}] / v[10]; may overlap: true
    PASS: v[10] / v[{1, 9}]; may overlap: false
    PASS: v[{1, 9}] / v[10]; may overlap: false
    PASS: v[k] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[k]; may overlap: true
    PASS: v[2] / v[idx]; may overlap: true
    PASS: v[idx] / v[2]; may overlap: true
    |}]

let%expect_test "Index overlap: Upfrom / All" =
  check_may_idx_overlap "3:" ":" ~expected:true;
  check_may_idx_overlap "1:" ":" ~expected:true;
  check_may_idx_overlap "10:" ":" ~expected:true;
  check_may_idx_overlap "k:" ":" ~expected:true;
  [%expect
    {|
    PASS: v[3:] / v[:]; may overlap: true
    PASS: v[:] / v[3:]; may overlap: true
    PASS: v[1:] / v[:]; may overlap: true
    PASS: v[:] / v[1:]; may overlap: true
    PASS: v[10:] / v[:]; may overlap: true
    PASS: v[:] / v[10:]; may overlap: true
    PASS: v[k:] / v[:]; may overlap: true
    PASS: v[:] / v[k:]; may overlap: true
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
    PASS: v[3:] / v[2]; may overlap: false
    PASS: v[2] / v[3:]; may overlap: false
    PASS: v[3:] / v[4]; may overlap: true
    PASS: v[4] / v[3:]; may overlap: true
    PASS: v[3:] / v[3]; may overlap: true
    PASS: v[3] / v[3:]; may overlap: true
    PASS: v[10:] / v[10]; may overlap: true
    PASS: v[10] / v[10:]; may overlap: true
    PASS: v[10:] / v[1]; may overlap: false
    PASS: v[1] / v[10:]; may overlap: false
    PASS: v[3:] / v[k]; may overlap: true
    PASS: v[k] / v[3:]; may overlap: true
    PASS: v[k:] / v[3]; may overlap: true
    PASS: v[3] / v[k:]; may overlap: true
    PASS: v[j:] / v[k]; may overlap: true
    PASS: v[k] / v[j:]; may overlap: true
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
    PASS: v[2:] / v[3:]; may overlap: true
    PASS: v[3:] / v[2:]; may overlap: true
    PASS: v[1:] / v[1:]; may overlap: true
    PASS: v[1:] / v[1:]; may overlap: true
    PASS: v[3:] / v[3:]; may overlap: true
    PASS: v[3:] / v[3:]; may overlap: true
    PASS: v[1:] / v[10:]; may overlap: true
    PASS: v[10:] / v[1:]; may overlap: true
    PASS: v[10:] / v[10:]; may overlap: true
    PASS: v[10:] / v[10:]; may overlap: true
    PASS: v[k:] / v[j:]; may overlap: true
    PASS: v[j:] / v[k:]; may overlap: true
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
    PASS: v[3:] / v[1:2]; may overlap: false
    PASS: v[1:2] / v[3:]; may overlap: false
    PASS: v[3:] / v[1:2]; may overlap: false
    PASS: v[1:2] / v[3:]; may overlap: false
    PASS: v[3:] / v[3:4]; may overlap: true
    PASS: v[3:4] / v[3:]; may overlap: true
    PASS: v[3:] / v[1:3]; may overlap: true
    PASS: v[1:3] / v[3:]; may overlap: true
    PASS: v[3:] / v[4:5]; may overlap: true
    PASS: v[4:5] / v[3:]; may overlap: true
    PASS: v[10:] / v[10:10]; may overlap: true
    PASS: v[10:10] / v[10:]; may overlap: true
    PASS: v[3:] / v[4:3]; may overlap: false
    PASS: v[4:3] / v[3:]; may overlap: false
    PASS: v[3:] / v[k:2]; may overlap: false
    PASS: v[k:2] / v[3:]; may overlap: false
    PASS: v[k:] / v[1:2]; may overlap: true
    PASS: v[1:2] / v[k:]; may overlap: true
    PASS: v[3:] / v[1:j]; may overlap: true
    PASS: v[1:j] / v[3:]; may overlap: true
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
    PASS: v[3:] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[3:]; may overlap: true
    PASS: v[3:] / v[{1, 2}]; may overlap: false
    PASS: v[{1, 2}] / v[3:]; may overlap: false
    PASS: v[3:] / v[{4, 5}]; may overlap: true
    PASS: v[{4, 5}] / v[3:]; may overlap: true
    PASS: v[10:] / v[{10, 1, 10}]; may overlap: true
    PASS: v[{10, 1, 10}] / v[10:]; may overlap: true
    PASS: v[10:] / v[{1, 9}]; may overlap: false
    PASS: v[{1, 9}] / v[10:]; may overlap: false
    PASS: v[k:] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[k:]; may overlap: true
    PASS: v[3:] / v[idx]; may overlap: true
    PASS: v[idx] / v[3:]; may overlap: true
    PASS: v[3:] / v[{1, k}]; may overlap: true
    PASS: v[{1, k}] / v[3:]; may overlap: true
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
    PASS: v[1:2] / v[:]; may overlap: true
    PASS: v[:] / v[1:2]; may overlap: true
    PASS: v[3:2] / v[:]; may overlap: false
    PASS: v[:] / v[3:2]; may overlap: false
    PASS: v[1:10] / v[:]; may overlap: true
    PASS: v[:] / v[1:10]; may overlap: true
    PASS: v[5:5] / v[:]; may overlap: true
    PASS: v[:] / v[5:5]; may overlap: true
    PASS: v[10:1] / v[:]; may overlap: false
    PASS: v[:] / v[10:1]; may overlap: false
    PASS: v[k:j] / v[:]; may overlap: true
    PASS: v[:] / v[k:j]; may overlap: true
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
    PASS: v[1:2] / v[2]; may overlap: true
    PASS: v[2] / v[1:2]; may overlap: true
    PASS: v[1:2] / v[3]; may overlap: false
    PASS: v[3] / v[1:2]; may overlap: false
    PASS: v[1:k] / v[1]; may overlap: true
    PASS: v[1] / v[1:k]; may overlap: true
    PASS: v[2:4] / v[2]; may overlap: true
    PASS: v[2] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[4]; may overlap: true
    PASS: v[4] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[1]; may overlap: false
    PASS: v[1] / v[2:4]; may overlap: false
    PASS: v[3:3] / v[3]; may overlap: true
    PASS: v[3] / v[3:3]; may overlap: true
    PASS: v[4:3] / v[3]; may overlap: false
    PASS: v[3] / v[4:3]; may overlap: false
    PASS: v[2:4] / v[k]; may overlap: true
    PASS: v[k] / v[2:4]; may overlap: true
    PASS: v[2:k] / v[1]; may overlap: false
    PASS: v[1] / v[2:k]; may overlap: false
    PASS: v[k:9] / v[10]; may overlap: false
    PASS: v[10] / v[k:9]; may overlap: false
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
    PASS: v[1:2] / v[3:]; may overlap: false
    PASS: v[3:] / v[1:2]; may overlap: false
    PASS: v[1:2] / v[3:]; may overlap: false
    PASS: v[3:] / v[1:2]; may overlap: false
    PASS: v[3:4] / v[3:]; may overlap: true
    PASS: v[3:] / v[3:4]; may overlap: true
    PASS: v[1:3] / v[3:]; may overlap: true
    PASS: v[3:] / v[1:3]; may overlap: true
    PASS: v[4:5] / v[3:]; may overlap: true
    PASS: v[3:] / v[4:5]; may overlap: true
    PASS: v[10:10] / v[10:]; may overlap: true
    PASS: v[10:] / v[10:10]; may overlap: true
    PASS: v[4:3] / v[3:]; may overlap: false
    PASS: v[3:] / v[4:3]; may overlap: false
    PASS: v[k:2] / v[3:]; may overlap: false
    PASS: v[3:] / v[k:2]; may overlap: false
    PASS: v[1:2] / v[k:]; may overlap: true
    PASS: v[k:] / v[1:2]; may overlap: true
    PASS: v[1:j] / v[3:]; may overlap: true
    PASS: v[3:] / v[1:j]; may overlap: true
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
    PASS: v[1:2] / v[3:4]; may overlap: false
    PASS: v[3:4] / v[1:2]; may overlap: false
    PASS: v[1:2] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[1:2]; may overlap: true
    PASS: v[1:k] / v[j:4]; may overlap: true
    PASS: v[j:4] / v[1:k]; may overlap: true
    PASS: v[k:2] / v[3:j]; may overlap: false
    PASS: v[3:j] / v[k:2]; may overlap: false
    PASS: v[3:2] / v[1:4]; may overlap: false
    PASS: v[1:4] / v[3:2]; may overlap: false
    PASS: v[2:4] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[2:4]; may overlap: true
    PASS: v[1:10] / v[2:3]; may overlap: true
    PASS: v[2:3] / v[1:10]; may overlap: true
    PASS: v[1:3] / v[3:5]; may overlap: true
    PASS: v[3:5] / v[1:3]; may overlap: true
    PASS: v[5:5] / v[1:10]; may overlap: true
    PASS: v[1:10] / v[5:5]; may overlap: true
    PASS: v[10:10] / v[10:10]; may overlap: true
    PASS: v[10:10] / v[10:10]; may overlap: true
    PASS: v[3:2] / v[4:3]; may overlap: false
    PASS: v[4:3] / v[3:2]; may overlap: false
    PASS: v[5:k] / v[j:4]; may overlap: false
    PASS: v[j:4] / v[5:k]; may overlap: false
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
    PASS: v[2:2] / v[{1, 3}]; may overlap: false
    PASS: v[{1, 3}] / v[2:2]; may overlap: false
    PASS: v[2:3] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[2:3]; may overlap: true
    PASS: v[1:2] / v[idx]; may overlap: true
    PASS: v[idx] / v[1:2]; may overlap: true
    PASS: v[2:4] / v[{2, 4}]; may overlap: true
    PASS: v[{2, 4}] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[{3}]; may overlap: true
    PASS: v[{3}] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[{1, 5}]; may overlap: false
    PASS: v[{1, 5}] / v[2:4]; may overlap: false
    PASS: v[2:4] / v[{4, 2, 4}]; may overlap: true
    PASS: v[{4, 2, 4}] / v[2:4]; may overlap: true
    PASS: v[3:2] / v[{2, 3}]; may overlap: false
    PASS: v[{2, 3}] / v[3:2]; may overlap: false
    PASS: v[2:4] / v[{k}]; may overlap: true
    PASS: v[{k}] / v[2:4]; may overlap: true
    PASS: v[k:2] / v[{3, 4}]; may overlap: false
    PASS: v[{3, 4}] / v[k:2]; may overlap: false
    PASS: v[3:j] / v[{1, 2}]; may overlap: false
    PASS: v[{1, 2}] / v[3:j]; may overlap: false
    |}]

let%expect_test "Index overlap: MultiIndex / All" =
  check_may_idx_overlap "{1, 3}" ":" ~expected:true;
  check_may_idx_overlap "{1}" ":" ~expected:true;
  check_may_idx_overlap "{10, 1, 10}" ":" ~expected:true;
  check_may_idx_overlap "idx" ":" ~expected:true;
  check_may_idx_overlap "{k, j}" ":" ~expected:true;
  [%expect
    {|
    PASS: v[{1, 3}] / v[:]; may overlap: true
    PASS: v[:] / v[{1, 3}]; may overlap: true
    PASS: v[{1}] / v[:]; may overlap: true
    PASS: v[:] / v[{1}]; may overlap: true
    PASS: v[{10, 1, 10}] / v[:]; may overlap: true
    PASS: v[:] / v[{10, 1, 10}]; may overlap: true
    PASS: v[idx] / v[:]; may overlap: true
    PASS: v[:] / v[idx]; may overlap: true
    PASS: v[{k, j}] / v[:]; may overlap: true
    PASS: v[:] / v[{k, j}]; may overlap: true
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
    PASS: v[{1, 3}] / v[2]; may overlap: false
    PASS: v[2] / v[{1, 3}]; may overlap: false
    PASS: v[{1, 3}] / v[3]; may overlap: true
    PASS: v[3] / v[{1, 3}]; may overlap: true
    PASS: v[{1, k}] / v[2]; may overlap: true
    PASS: v[2] / v[{1, k}]; may overlap: true
    PASS: v[{1}] / v[1]; may overlap: true
    PASS: v[1] / v[{1}]; may overlap: true
    PASS: v[{1}] / v[2]; may overlap: false
    PASS: v[2] / v[{1}]; may overlap: false
    PASS: v[{10, 1, 10}] / v[10]; may overlap: true
    PASS: v[10] / v[{10, 1, 10}]; may overlap: true
    PASS: v[{1, 9}] / v[10]; may overlap: false
    PASS: v[10] / v[{1, 9}]; may overlap: false
    PASS: v[{1, 3}] / v[k]; may overlap: true
    PASS: v[k] / v[{1, 3}]; may overlap: true
    PASS: v[idx] / v[2]; may overlap: true
    PASS: v[2] / v[idx]; may overlap: true
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
    PASS: v[{1, 3}] / v[3:]; may overlap: true
    PASS: v[3:] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 2}] / v[3:]; may overlap: false
    PASS: v[3:] / v[{1, 2}]; may overlap: false
    PASS: v[{4, 5}] / v[3:]; may overlap: true
    PASS: v[3:] / v[{4, 5}]; may overlap: true
    PASS: v[{10, 1, 10}] / v[10:]; may overlap: true
    PASS: v[10:] / v[{10, 1, 10}]; may overlap: true
    PASS: v[{1, 9}] / v[10:]; may overlap: false
    PASS: v[10:] / v[{1, 9}]; may overlap: false
    PASS: v[{1, 3}] / v[k:]; may overlap: true
    PASS: v[k:] / v[{1, 3}]; may overlap: true
    PASS: v[idx] / v[3:]; may overlap: true
    PASS: v[3:] / v[idx]; may overlap: true
    PASS: v[{1, k}] / v[3:]; may overlap: true
    PASS: v[3:] / v[{1, k}]; may overlap: true
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
    PASS: v[{1, 3}] / v[2:2]; may overlap: false
    PASS: v[2:2] / v[{1, 3}]; may overlap: false
    PASS: v[{1, 3}] / v[2:3]; may overlap: true
    PASS: v[2:3] / v[{1, 3}]; may overlap: true
    PASS: v[idx] / v[1:2]; may overlap: true
    PASS: v[1:2] / v[idx]; may overlap: true
    PASS: v[{2, 4}] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[{2, 4}]; may overlap: true
    PASS: v[{3}] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[{3}]; may overlap: true
    PASS: v[{1, 5}] / v[2:4]; may overlap: false
    PASS: v[2:4] / v[{1, 5}]; may overlap: false
    PASS: v[{4, 2, 4}] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[{4, 2, 4}]; may overlap: true
    PASS: v[{2, 3}] / v[3:2]; may overlap: false
    PASS: v[3:2] / v[{2, 3}]; may overlap: false
    PASS: v[{k}] / v[2:4]; may overlap: true
    PASS: v[2:4] / v[{k}]; may overlap: true
    PASS: v[{3, 4}] / v[k:2]; may overlap: false
    PASS: v[k:2] / v[{3, 4}]; may overlap: false
    PASS: v[{1, 2}] / v[3:j]; may overlap: false
    PASS: v[3:j] / v[{1, 2}]; may overlap: false
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
    PASS: v[{1, 3}] / v[{2, 4}]; may overlap: false
    PASS: v[{2, 4}] / v[{1, 3}]; may overlap: false
    PASS: v[{1, 3}] / v[{3, 4}]; may overlap: true
    PASS: v[{3, 4}] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 1}] / v[{2, 2}]; may overlap: false
    PASS: v[{2, 2}] / v[{1, 1}]; may overlap: false
    PASS: v[idx] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[idx]; may overlap: true
    PASS: v[{1, 3}] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[{1, 3}]; may overlap: true
    PASS: v[{3, 1}] / v[{1, 3}]; may overlap: true
    PASS: v[{1, 3}] / v[{3, 1}]; may overlap: true
    PASS: v[{1, 1}] / v[{1, 1}]; may overlap: true
    PASS: v[{1, 1}] / v[{1, 1}]; may overlap: true
    PASS: v[{1}] / v[{2}]; may overlap: false
    PASS: v[{2}] / v[{1}]; may overlap: false
    PASS: v[{1}] / v[{1}]; may overlap: true
    PASS: v[{1}] / v[{1}]; may overlap: true
    PASS: v[idx] / v[idx]; may overlap: true
    PASS: v[idx] / v[idx]; may overlap: true
    PASS: v[{k}] / v[{j}]; may overlap: true
    PASS: v[{j}] / v[{k}]; may overlap: true
    PASS: v[{1, k}] / v[{2, 3}]; may overlap: true
    PASS: v[{2, 3}] / v[{1, k}]; may overlap: true
    |}]

let%expect_test "Different columns remain disjoint after All" =
  check_read_overlap
    {|model { matrix[4, 4] v; print(v[:, 1]); print(v[:, 2]); }|} false;
  [%expect
    {|
    PASS: v[:, 1] / v[:, 2]; may overlap: false
    PASS: v[:, 2] / v[:, 1]; may overlap: false
    |}]

let%expect_test "Overlapping row ranges share a column" =
  check_read_overlap
    {|model { matrix[4, 4] v; print(v[1:2, 1]); print(v[2:3, 1]); }|} true;
  [%expect
    {|
    PASS: v[1:2, 1] / v[2:3, 1]; may overlap: true
    PASS: v[2:3, 1] / v[1:2, 1]; may overlap: true
    |}]

let%expect_test "Different columns separate unknown row selections" =
  check_read_overlap
    {|data { int k; array[2] int idx; }
      model { matrix[4, 4] v; print(v[idx, 1]); print(v[k:4, 2]); }|}
    false;
  [%expect
    {|
    PASS: v[idx, 1] / v[k:4, 2]; may overlap: false
    PASS: v[k:4, 2] / v[idx, 1]; may overlap: false
    |}]

let%expect_test "Disjoint column ranges separate matching row arrays" =
  check_read_overlap
    {|model { matrix[4, 4] v; print(v[{1, 3}, 1:2]); print(v[{1, 3}, 3:4]); }|}
    false;
  [%expect
    {|
    PASS: v[{1, 3}, 1:2] / v[{1, 3}, 3:4]; may overlap: false
    PASS: v[{1, 3}, 3:4] / v[{1, 3}, 1:2]; may overlap: false
    |}]

let%expect_test "A whole column overlaps a whole row at their intersection" =
  check_read_overlap
    {|model { matrix[4, 4] v; print(v[:, 1]); print(v[1, :]); }|} true;
  [%expect
    {|
    PASS: v[:, 1] / v[1, :]; may overlap: true
    PASS: v[1, :] / v[:, 1]; may overlap: true
    |}]

let%expect_test "Nested slice: position one overlaps original element two" =
  (* The access collector conservatively retains v[2:4], not just v[2]. *)
  check_read_overlap {|model { vector[4] v; print(v[2:4][1]); print(v[2]); }|}
    true;
  [%expect
    {|
    PASS: v[2:4] / v[2]; may overlap: true
    PASS: v[2] / v[2:4]; may overlap: true
    |}]

let%expect_test "Nested slice: an element outside the base slice is disjoint" =
  check_read_overlap {|model { vector[4] v; print(v[2:4][1]); print(v[1]); }|}
    false;
  [%expect
    {|
    PASS: v[2:4] / v[1]; may overlap: false
    PASS: v[1] / v[2:4]; may overlap: false
    |}]

let%expect_test "Nested slice: the retained base slice can overestimate overlap"
    =
  (* v[2:4][1] selects element 2, but the collector retains the whole slice. Its
     possible overlap with v[3] is intentionally conservative. *)
  check_read_overlap {|model { vector[4] v; print(v[2:4][1]); print(v[3]); }|}
    true;
  [%expect
    {|
    PASS: v[2:4] / v[3]; may overlap: true
    PASS: v[3] / v[2:4]; may overlap: true
    |}]

let%expect_test "Empty selections never overlap any index form" =
  let empty_array =
    Expr.Helpers.internal_funapp FnMakeArray []
      {Expr.Typed.Meta.empty with type_= UArray UInt} in
  let unknown_array =
    { (Expr.Helpers.variable "idx") with
      meta= {Expr.Typed.Meta.empty with type_= UArray UInt} } in
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
    PASS: v[{}] / v[:]; may overlap: false
    PASS: v[:] / v[{}]; may overlap: false
    PASS: v[{}] / v[2]; may overlap: false
    PASS: v[2] / v[{}]; may overlap: false
    PASS: v[{}] / v[2:]; may overlap: false
    PASS: v[2:] / v[{}]; may overlap: false
    PASS: v[{}] / v[1:3]; may overlap: false
    PASS: v[1:3] / v[{}]; may overlap: false
    PASS: v[{}] / v[{2}]; may overlap: false
    PASS: v[{2}] / v[{}]; may overlap: false
    PASS: v[{}] / v[idx]; may overlap: false
    PASS: v[idx] / v[{}]; may overlap: false
    PASS: v[{}] / v[{}]; may overlap: false
    PASS: v[{}] / v[{}]; may overlap: false
    PASS: v[3:2] / v[:]; may overlap: false
    PASS: v[:] / v[3:2]; may overlap: false
    PASS: v[3:2] / v[2]; may overlap: false
    PASS: v[2] / v[3:2]; may overlap: false
    PASS: v[3:2] / v[2:]; may overlap: false
    PASS: v[2:] / v[3:2]; may overlap: false
    PASS: v[3:2] / v[1:3]; may overlap: false
    PASS: v[1:3] / v[3:2]; may overlap: false
    PASS: v[3:2] / v[{2}]; may overlap: false
    PASS: v[{2}] / v[3:2]; may overlap: false
    PASS: v[3:2] / v[idx]; may overlap: false
    PASS: v[idx] / v[3:2]; may overlap: false
    PASS: v[3:2] / v[{}]; may overlap: false
    PASS: v[{}] / v[3:2]; may overlap: false
    |}]
