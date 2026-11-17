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

let location_text (location : Access.location) =
  let step_text = function
    | Access.Field field -> Fmt.str ".%d" field
    | Subscript index -> Fmt.str "[%a]" (Index.pp Expr.Typed.pp) index in
  location.variable
  ^ String.concat ~sep:"" (List.map location.path ~f:step_text)

let print_locations name locations =
  let paths =
    List.sort ~cmp:String.compare (List.map locations ~f:location_text) in
  Fmt.pr "%s: %s@." name (String.concat ~sep:", " paths)

let%expect_test "Collect indexed reads, writes, tuple fields and slices" =
  assignment_accesses
    {|model {
        vector[4] v; vector[4] w; array[4] int idx;
        tuple(vector[4], real) t;
        v[idx[1]] = w[idx[2] * idx[3]];
        t.1[idx[1]] = v[2];
        v[1:2] = w[2:3];
        v[1] += 1;
        real z = w[2:3][1];
      }|}
  |> List.iter ~f:(fun accesses ->
      print_locations "reads" accesses.Access.reads;
      print_locations "writes" accesses.writes);
  [%expect
    {|
    reads: idx[1], idx[2], idx[3], w[(idx[2] * idx[3])]
    writes: v[idx[1]]
    reads: idx[1], v[2]
    writes: t.1[idx[1]]
    reads: w[2:3]
    writes: v[1:2]
    reads: v[1]
    writes: v[1]
    reads: w[2:3]
    writes: z
    |}]

let write_paths source =
  List.concat_map (assignment_accesses source) ~f:(fun accesses ->
      List.map accesses.Access.writes ~f:(fun location -> location.Access.path))

let check_paths defined_vars source expected_overlap expected_overwrite =
  match write_paths source with
  | [first; second] ->
      assert (Access.may_overlap first second = expected_overlap);
      assert (Access.may_overlap second first = expected_overlap);
      assert (
        Access.definitely_overwrites defined_vars first second
        = expected_overwrite)
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
      check_paths (Set.Poly.singleton "v") source overlap overwrite);
  [%expect {| |}]

let%expect_test "Mutable and random indices cannot prove overwrites" =
  check_paths
    (Set.Poly.of_list ["v"; "k"])
    {|data { int k; } model { vector[4] v; v[k] = 1; v[k] = 2; }|} true false;
  check_paths
    (Set.Poly.of_list ["v"; "idx"])
    {|data { int k; array[4] int idx; } model { vector[4] v; v[idx[k]] = 1; v[idx[k]] = 2; }|}
    true false;
  check_paths (Set.Poly.singleton "v")
    {|functions { void f_rng(vector theta) {
        vector[4] v; v[categorical_rng(theta)] = 1; v[categorical_rng(theta)] = 2;
      }} model {}|}
    true false;
  [%expect {| |}]

let%expect_test "Tuple fields and whole-variable writes" =
  check_paths Set.Poly.empty
    {|model { tuple(real, real) t; t.1 = 1; t.2 = 2; }|} false false;
  check_paths Set.Poly.empty
    {|model { vector[4] v; v = rep_vector(0, 4); v[1] = 1; }|} true true;
  check_paths Set.Poly.empty
    {|model { vector[4] v; v[1:2] = rep_vector(0, 2); v[3:4] = rep_vector(0, 2); }|}
    true false;
  [%expect {| |}]

let%expect_test "Loop headers write the induction variable" =
  let statements =
    statement_map {|data { int N; } model { for (n in 1:N) print(n); }|} in
  LabelMap.iter statements ~f:(fun ~key:_ ~data:(pattern, _) ->
      match pattern with
      | Stmt.Pattern.For _ ->
          let accesses = Access.collect_accesses pattern in
          print_locations "reads" accesses.reads;
          print_locations "writes" accesses.writes
      | _ -> ());
  [%expect {|
    reads: N
    writes: n
    |}]

let%expect_test "Target-modifying calls retain the target read and write" =
  assignment_accesses
    {|functions { real f_lp(real a) { target += a; return a; } }
      parameters { real a; } model { real b = f_lp(a); }|}
  |> List.iter ~f:(fun accesses ->
      print_locations "reads" accesses.Access.reads;
      print_locations "writes" accesses.writes);
  [%expect {|
    reads: a, target
    writes: b, target
    |}]
