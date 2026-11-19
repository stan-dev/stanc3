open Std
open Middle
open Analysis_and_optimization.Dataflow_types
open Analysis_and_optimization.Dataflow_utils
module Access = Analysis_and_optimization.Dependence

let statement_map source =
  (* Trim the leading newline so the printed line numbers start at the first
     line of Stan code in each test. *)
  let mir = Test_utils.mir_of_string (String.trim source) in
  let bodies =
    List.filter_map mir.functions_block ~f:(fun definition ->
        definition.Program.fdbody) in
  build_statement_map
    (fun Stmt.{pattern; _} -> pattern)
    (fun Stmt.{meta; _} -> meta)
    Stmt.{meta= Location_span.empty; pattern= SList (bodies @ mir.log_prob)}

let pp_index_expr ppf (expr : Expr.Typed.t) =
  match Expr.Helpers.array_literal_elements expr with
  | Some elements ->
      Fmt.pf ppf "{%a}" (Fmt.list ~sep:Fmt.comma Expr.Typed.pp) elements
  | None -> Expr.Typed.pp ppf expr

let location_text (location : Access.location) =
  Fmt.str "%s%a" location.variable
    (Access.pp_access_path pp_index_expr)
    location.path

type statement_accesses =
  {statement: string; reads: string list; writes: string list}

let check_accesses ~statements:kind ~expected source =
  let statements = statement_map source in
  let lines = String.split_lines (String.trim source) |> Array.of_list in
  let paths locations =
    List.map locations ~f:location_text |> List.sort ~cmp:String.compare in
  let actual =
    LabelMap.bindings statements
    |> List.filter_map ~f:(fun (_, (pattern, loc)) ->
        match (kind, pattern) with
        | `Assignments, Stmt.Pattern.Assignment _ | `Loop_headers, For _ ->
            let accesses = Access.collect_accesses pattern in
            let line = loc.Location_span.begin_loc.line_num in
            Some
              ( line
              , { statement= String.trim lines.(line - 1)
                ; reads= paths accesses.reads
                ; writes= paths accesses.writes } )
        | _ -> None) in
  let show = function [] -> "none" | xs -> String.concat ~sep:", " xs in
  let pp ppf {statement; reads; writes} =
    Fmt.pf ppf "%s@,  reads: %s@,  writes: %s" statement (show reads)
      (show writes) in
  List.iter actual ~f:(fun (line, row) ->
      Fmt.pr "@[<v>line %d: %a@]@." line pp row);
  let actual = List.map actual ~f:snd in
  if actual <> expected then
    let pp_rows = Fmt.list ~sep:Fmt.cut pp in
    Fmt.failwith "@[<v>Expected accesses:@,%a@,Got:@,%a@]" pp_rows expected
      pp_rows actual

let%expect_test
    "Access collection: index expressions contribute their own reads" =
  check_accesses ~statements:`Assignments
    ~expected:
      [ { statement= "v[idx[1]] = w[idx[2] * idx[3]];"
        ; reads= ["idx[1]"; "idx[2]"; "idx[3]"; "w[(idx[2] * idx[3])]"]
        ; writes= ["v[idx[1]]"] } ]
    {|
      data {
        array[3] int<lower=1, upper=2> idx;
        vector[4] w;
      }
      model {
        vector[4] v;
        v[idx[1]] = w[idx[2] * idx[3]];
      }
    |};
  [%expect
    {|
    line 7: v[idx[1]] = w[idx[2] * idx[3]];
      reads: idx[1], idx[2], idx[3], w[(idx[2] * idx[3])]
      writes: v[idx[1]]
    |}]

let%expect_test "Access collection: tuple fields precede element indices" =
  check_accesses ~statements:`Assignments
    ~expected:
      [ { statement= "t.1[idx[1]] = v[2];"
        ; reads= ["idx[1]"; "v[2]"]
        ; writes= ["t.1[idx[1]]"] } ]
    {|
      data {
        array[1] int<lower=1, upper=4> idx;
        vector[4] v;
      }
      model {
        tuple(vector[4], real) t;
        t.1[idx[1]] = v[2];
      }
    |};
  [%expect
    {|
    line 7: t.1[idx[1]] = v[2];
      reads: idx[1], v[2]
      writes: t.1[idx[1]]
    |}]

let%expect_test "Access collection: tuple fields have separate paths" =
  check_accesses ~statements:`Assignments
    ~expected:
      [{statement= "t.1 = source.2;"; reads= ["source.2"]; writes= ["t.1"]}]
    {|
      data {
        tuple(real, real) source;
      }
      model {
        tuple(real, real) t;
        t.1 = source.2;
      }
    |};
  [%expect
    {|
    line 6: t.1 = source.2;
      reads: source.2
      writes: t.1
    |}]

let%expect_test
    "Access collection: tuple element reads keep the field and index" =
  check_accesses ~statements:`Assignments
    ~expected:[{statement= "real x = t.1[2];"; reads= ["t.1[2]"]; writes= ["x"]}]
    {|
      data {
        tuple(vector[4], real) t;
      }
      model {
        real x = t.1[2];
      }
    |};
  [%expect
    {|
    line 5: real x = t.1[2];
      reads: t.1[2]
      writes: x
    |}]

let%expect_test "Access collection: array elements and nested tuple fields" =
  check_accesses ~statements:`Assignments
    ~expected:
      [ { statement= "t[2].1.2[3] = source[1].1.2[2];"
        ; reads= ["source[1].1.2[2]"]
        ; writes= ["t[2].1.2[3]"] } ]
    {|
      data {
        array[2] tuple(tuple(real, vector[3]), real) source;
      }
      model {
        array[2] tuple(tuple(real, vector[3]), real) t;
        t[2].1.2[3] = source[1].1.2[2];
      }
    |};
  [%expect
    {|
    line 6: t[2].1.2[3] = source[1].1.2[2];
      reads: source[1].1.2[2]
      writes: t[2].1.2[3]
    |}]

let%expect_test "Access collection: slices retain their bounds" =
  check_accesses ~statements:`Assignments
    ~expected:
      [{statement= "v[1:2] = w[2:3];"; reads= ["w[2:3]"]; writes= ["v[1:2]"]}]
    {|
      data {
        vector[4] w;
      }
      model {
        vector[4] v;
        v[1:2] = w[2:3];
      }
    |};
  [%expect
    {|
    line 6: v[1:2] = w[2:3];
      reads: w[2:3]
      writes: v[1:2]
    |}]

let%expect_test "Access collection: compound assignment reads its destination" =
  check_accesses ~statements:`Assignments
    ~expected:
      [ {statement= "vector[4] v = initial;"; reads= ["initial"]; writes= ["v"]}
      ; {statement= "v[1] += 1;"; reads= ["v[1]"]; writes= ["v[1]"]} ]
    {|
      parameters {
        vector[4] initial;
      }
      model {
        vector[4] v = initial;
        v[1] += 1;
      }
    |};
  [%expect
    {|
    line 5: vector[4] v = initial;
      reads: initial
      writes: v
    line 6: v[1] += 1;
      reads: v[1]
      writes: v[1]
    |}]

let%expect_test "Access collection: nested slices retain the base selection" =
  check_accesses ~statements:`Assignments
    ~expected:
      [{statement= "real z = w[2:3][1];"; reads= ["w[2:3]"]; writes= ["z"]}]
    {|
      data {
        vector[4] w;
      }
      model {
        real z = w[2:3][1];
      }
    |};
  [%expect
    {|
    line 5: real z = w[2:3][1];
      reads: w[2:3]
      writes: z
    |}]

let check_overlap first_path second_path expected =
  let actual = Access.may_overlap first_path second_path in
  if actual <> expected then
    Fmt.failwith "Overlap of v%a and v%a: expected %b, got %b"
      (Access.pp_access_path pp_index_expr)
      first_path
      (Access.pp_access_path pp_index_expr)
      second_path expected actual

let check_write_comparison variables ~previous_line ~next_line ~may_overlap
    ~previous_overwritten source =
  let statements = statement_map source |> LabelMap.bindings in
  let written_vars =
    List.fold_left statements ~init:Set.Poly.empty
      ~f:(fun names (_, (pattern, _)) ->
        List.fold_left (Access.collect_accesses pattern).writes ~init:names
          ~f:(fun collected write ->
            Set.Poly.add write.Access.variable collected)) in
  let write_on_line line =
    let writes =
      List.concat_map statements ~f:(fun (_, (pattern, location)) ->
          match pattern with
          | Stmt.Pattern.Assignment _
            when location.Location_span.begin_loc.line_num = line ->
              List.filter (Access.collect_accesses pattern).writes
                ~f:(fun write -> List.mem write.Access.variable ~set:variables)
          | _ -> []) in
    match writes with
    | [write] -> write
    | _ ->
        Fmt.failwith "Expected one write to %s on line %d, found %d in:\n%s"
          (String.concat ~sep:", " variables)
          line (List.length writes) (String.trim source) in
  if previous_line >= next_line then
    Fmt.failwith "Expected previous line %d to precede next line %d"
      previous_line next_line;
  let previous = write_on_line previous_line in
  let next_write = write_on_line next_line in
  let actual_overlap =
    Access.Location.same_var previous next_write
    && Access.may_overlap previous.path next_write.path in
  let actual_overwritten =
    Access.is_overwritten written_vars previous next_write in
  let check name expected actual =
    if expected <> actual then
      let lines = String.split_lines (String.trim source) |> Array.of_list in
      Fmt.failwith
        "%s: expected %b, got %b\n\
        \  previous (line %d): %s\n\
        \  next (line %d): %s"
        name expected actual previous_line
        (String.trim lines.(previous_line - 1))
        next_line
        (String.trim lines.(next_line - 1)) in
  check "may_overlap" may_overlap actual_overlap;
  check "previous_overwritten" previous_overwritten actual_overwritten

let%test_unit "Overwrite: different literal elements retain the earlier write" =
  check_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        v[1] = 1;
        v[2] = 2;
      }
    |}

let%test_unit "Overwrite: the same literal element replaces the earlier write" =
  check_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:true
    {|
      model {
        vector[4] v;
        v[1] = 1;
        v[1] = 2;
      }
    |}

let%test_unit "Overwrite: the same stable index replaces the earlier write" =
  check_write_comparison ["v"] ~previous_line:6 ~next_line:7 ~may_overlap:true
    ~previous_overwritten:true
    {|
      data {
        int<lower=1, upper=4> k;
      }
      model {
        vector[4] v;
        v[k] = 1;
        v[k] = 2;
      }
    |}

let%test_unit "Overwrite: different symbolic indices can alias" =
  check_write_comparison ["v"] ~previous_line:7 ~next_line:8 ~may_overlap:true
    ~previous_overwritten:false
    {|
      data {
        int<lower=1, upper=4> k;
        int<lower=1, upper=4> j;
      }
      model {
        vector[4] v;
        v[k] = 1;
        v[j] = 2;
      }
    |}

let%test_unit "Overwrite: algebraic equivalence is not yet used" =
  check_write_comparison ["v"] ~previous_line:6 ~next_line:7 ~may_overlap:true
    ~previous_overwritten:false
    {|
      data {
        int<lower=1, upper=2> k;
      }
      model {
        vector[4] v;
        v[k + k] = 1;
        v[2 * k] = 2;
      }
    |}

let%test_unit "Overwrite: stable indirect indices replace the earlier write" =
  check_write_comparison ["v"] ~previous_line:7 ~next_line:8 ~may_overlap:true
    ~previous_overwritten:true
    {|
      data {
        int<lower=1, upper=4> k;
        array[4] int<lower=1, upper=4> idx;
      }
      model {
        vector[4] v;
        v[idx[k]] = 1;
        v[idx[k]] = 2;
      }
    |}

let%test_unit "Overwrite: changing k in v[idx[k]] retains the earlier write" =
  check_write_comparison ["v"] ~previous_line:5 ~next_line:7 ~may_overlap:true
    ~previous_overwritten:false
    {|
      data { array[2] int<lower=1, upper=4> idx; }
      model {
        vector[4] v;
        int k = 1;
        v[idx[k]] = 1;
        k = 2;
        v[idx[k]] = 2;
      }
    |}

let%test_unit "Overwrite: nonlinear indices can alias a symbolic element" =
  check_write_comparison ["v"] ~previous_line:7 ~next_line:8 ~may_overlap:true
    ~previous_overwritten:false
    {|
      data {
        int<lower=1, upper=2> k;
        int<lower=1, upper=4> j;
      }
      model {
        vector[4] v;
        v[k * k] = 1;
        v[j] = 2;
      }
    |}

let%test_unit "Overwrite: changing the scalar index retains the earlier write" =
  check_write_comparison ["v"] ~previous_line:4 ~next_line:6 ~may_overlap:true
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        int k = 1;
        v[k] = 1;
        k = 2;
        v[k] = 2;
      }
    |}

let%test_unit "Overwrite: changing the index array retains the earlier write" =
  check_write_comparison ["v"] ~previous_line:4 ~next_line:6 ~may_overlap:true
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        array[1] int idx = {1};
        v[idx[1]] = 1;
        idx[1] = 2;
        v[idx[1]] = 2;
      }
    |}

let%test_unit "Overwrite: different tuple fields remain distinct" =
  check_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        tuple(real, real) t;
        t.1 = 1;
        t.2 = 2;
      }
    |}

let%test_unit "Overwrite: an element write does not replace the whole vector" =
  check_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        v = rep_vector(1, 4);
        v[1] = 2;
      }
    |}

let%test_unit "Overwrite: a whole-vector write replaces an element" =
  check_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:true
    {|
      model {
        vector[4] v;
        v[1] = 1;
        v = rep_vector(2, 4);
      }
    |}

let%test_unit "Overwrite: different variables remain distinct" =
  check_write_comparison ["v"; "w"] ~previous_line:4 ~next_line:5
    ~may_overlap:false ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        vector[4] w;
        v[1] = 1;
        w = rep_vector(2, 4);
      }
    |}

let%test_unit "Overwrite: whole writes to different variables remain distinct" =
  check_write_comparison ["v"; "w"] ~previous_line:4 ~next_line:5
    ~may_overlap:false ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        vector[4] w;
        v = rep_vector(1, 4);
        w = rep_vector(2, 4);
      }
    |}

let%test_unit "Overwrite: disjoint slices retain the earlier write" =
  check_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        v[1:2] = rep_vector(1, 2);
        v[3:4] = rep_vector(2, 2);
      }
    |}

let%test_unit
    "Overwrite: tuple fields in different array elements remain distinct" =
  check_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        array[2] tuple(real, real) t;
        t[1].2 = 1;
        t[2].1 = 2;
      }
    |}

let%test_unit
    "Overwrite: tuple fields in the same array element remain distinct" =
  check_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        array[2] tuple(real, real) t;
        t[1].1 = 1;
        t[1].2 = 2;
      }
    |}

let%test_unit "Overwrite: the same tuple field replaces the earlier write" =
  check_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:true
    {|
      model {
        array[2] tuple(real, real) t;
        t[1].2 = 1;
        t[1].2 = 2;
      }
    |}

let%test_unit "Overwrite: an element does not replace its enclosing tuple field"
    =
  check_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:false
    {|
      model {
        tuple(vector[4], real) t;
        t.1 = rep_vector(1, 4);
        t.1[2] = 2;
      }
    |}

let%test_unit "Overwrite: a tuple field replaces its contained element" =
  check_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:true
    {|
      model {
        tuple(vector[4], real) t;
        t.1[2] = 1;
        t.1 = rep_vector(2, 4);
      }
    |}

let%test_unit "Overwrite: repeated RNG calls can select different elements" =
  check_write_comparison ["v"] ~previous_line:4 ~next_line:5 ~may_overlap:true
    ~previous_overwritten:false
    {|
      functions {
        void f_rng(vector theta) {
          vector[num_elements(theta)] v;
          v[categorical_rng(theta)] = 1;
          v[categorical_rng(theta)] = 2;
        }
      }
      model {}
    |}

let%expect_test "Loop headers write the induction variable" =
  check_accesses ~statements:`Loop_headers
    ~expected:[{statement= "for (n in 1:N) {"; reads= ["N"]; writes= ["n"]}]
    {|
        data {
          int N;
        }
        model {
          for (n in 1:N) {
            print(n);
          }
        }
      |};
  [%expect
    {|
    line 5: for (n in 1:N) {
      reads: N
      writes: n
    |}]

let%expect_test "Target-modifying calls retain the target read and write" =
  check_accesses ~statements:`Assignments
    ~expected:
      [ { statement= "real b = f_lp(a);"
        ; reads= ["a"; "target"]
        ; writes= ["b"; "target"] } ]
    {|
      functions {
        real f_lp(real a) {
          target += a;
          return a;
        }
      }
      parameters {
        real a;
      }
      model {
        real b = f_lp(a);
      }
    |};
  [%expect
    {|
    line 11: real b = f_lp(a);
      reads: a, target
      writes: b, target
    |}]

let check_read_overlap ~expected source =
  let reads =
    statement_map source |> LabelMap.bindings
    |> List.concat_map ~f:(fun (_, (pattern, _)) ->
        (Access.collect_accesses pattern).reads)
    |> List.filter ~f:(fun read -> String.equal read.Access.variable "v") in
  match reads with
  | [first; second] ->
      check_overlap first.path second.path expected;
      check_overlap second.path first.path expected
  | _ ->
      Fmt.failwith "Expected two reads of v, got %d in:\n%s" (List.length reads)
        (String.trim source)

let index_read_program ~container_type first_index second_index =
  Fmt.str
    {|
        data {
          int k;
          int j;
          array[10] int idx;
        }
        model {
          %s v;
          print(v[%s]);
          print(v[%s]);
        }
      |}
    container_type first_index second_index

let vector_reads = index_read_program ~container_type:"vector[10]"
let matrix_reads = index_read_program ~container_type:"matrix[4, 4]"

let%test_unit "Index overlap: All / All" =
  check_read_overlap (vector_reads ":" ":") ~expected:true

let%test_unit "Index overlap: All / Single" =
  check_read_overlap (vector_reads ":" "2") ~expected:true;
  check_read_overlap (vector_reads ":" "1") ~expected:true;
  check_read_overlap (vector_reads ":" "10") ~expected:true;
  check_read_overlap (vector_reads ":" "k") ~expected:true

let%test_unit "Index overlap: All / Upfrom" =
  check_read_overlap (vector_reads ":" "3:") ~expected:true;
  check_read_overlap (vector_reads ":" "1:") ~expected:true;
  check_read_overlap (vector_reads ":" "10:") ~expected:true;
  check_read_overlap (vector_reads ":" "k:") ~expected:true

let%test_unit "Index overlap: All / Between" =
  check_read_overlap (vector_reads ":" "1:2") ~expected:true;
  check_read_overlap (vector_reads ":" "3:2") ~expected:false;
  check_read_overlap (vector_reads ":" "1:10") ~expected:true;
  check_read_overlap (vector_reads ":" "5:5") ~expected:true;
  check_read_overlap (vector_reads ":" "10:1") ~expected:false;
  check_read_overlap (vector_reads ":" "k:j") ~expected:true

let%test_unit "Index overlap: All / MultiIndex" =
  check_read_overlap (vector_reads ":" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads ":" "{1}") ~expected:true;
  check_read_overlap (vector_reads ":" "{10, 1, 10}") ~expected:true;
  check_read_overlap (vector_reads ":" "idx") ~expected:true;
  check_read_overlap (vector_reads ":" "{k, j}") ~expected:true

let%test_unit "Index overlap: Single / All" =
  check_read_overlap (vector_reads "2" ":") ~expected:true;
  check_read_overlap (vector_reads "1" ":") ~expected:true;
  check_read_overlap (vector_reads "10" ":") ~expected:true;
  check_read_overlap (vector_reads "k" ":") ~expected:true

let%test_unit "Index overlap: Single / Single" =
  check_read_overlap (vector_reads "1" "1") ~expected:true;
  check_read_overlap (vector_reads "1" "2") ~expected:false;
  check_read_overlap (vector_reads "2 * k" "2 * k") ~expected:true;
  check_read_overlap (vector_reads "k" "j") ~expected:true;
  check_read_overlap (vector_reads "2 * k" "2 * k + 1") ~expected:false;
  check_read_overlap (vector_reads "10" "10") ~expected:true;
  check_read_overlap (vector_reads "1" "10") ~expected:false;
  check_read_overlap (vector_reads "1" "k") ~expected:true;
  check_read_overlap (vector_reads "k" "k") ~expected:true;
  check_read_overlap (vector_reads "k + 1 * j" "k + 1 * j") ~expected:true

let%test_unit "Index overlap: integer parity" =
  check_read_overlap (vector_reads "2 * k" "2 * j + 1") ~expected:false;
  check_read_overlap (vector_reads "k * 2" "2 * j - 1") ~expected:false;
  check_read_overlap (vector_reads "-(2 * k)" "2 * j + 1") ~expected:false;
  check_read_overlap (vector_reads "2 * k + 1" "2 * j + 3") ~expected:true;
  check_read_overlap (vector_reads "2 * k" "2 * j") ~expected:true;
  check_read_overlap (vector_reads "k * j" "2") ~expected:true;
  check_read_overlap (vector_reads "k * j" "3") ~expected:true;
  check_read_overlap (vector_reads "k + 1" "2 * j") ~expected:true

let%test_unit "Index overlap: Single / Upfrom" =
  check_read_overlap (vector_reads "2" "3:") ~expected:false;
  check_read_overlap (vector_reads "4" "3:") ~expected:true;
  check_read_overlap (vector_reads "3" "3:") ~expected:true;
  check_read_overlap (vector_reads "10" "10:") ~expected:true;
  check_read_overlap (vector_reads "1" "10:") ~expected:false;
  check_read_overlap (vector_reads "k" "3:") ~expected:true;
  check_read_overlap (vector_reads "3" "k:") ~expected:true;
  check_read_overlap (vector_reads "k" "j:") ~expected:true

let%test_unit "Index overlap: Single / Between" =
  check_read_overlap (vector_reads "2" "1:2") ~expected:true;
  check_read_overlap (vector_reads "3" "1:2") ~expected:false;
  check_read_overlap (vector_reads "1" "1:k") ~expected:true;
  check_read_overlap (vector_reads "2" "2:4") ~expected:true;
  check_read_overlap (vector_reads "4" "2:4") ~expected:true;
  check_read_overlap (vector_reads "1" "2:4") ~expected:false;
  check_read_overlap (vector_reads "3" "3:3") ~expected:true;
  check_read_overlap (vector_reads "3" "4:3") ~expected:false;
  check_read_overlap (vector_reads "k" "2:4") ~expected:true;
  check_read_overlap (vector_reads "1" "2:k") ~expected:false;
  check_read_overlap (vector_reads "10" "k:9") ~expected:false

let%test_unit "Index overlap: Single / MultiIndex" =
  check_read_overlap (vector_reads "2" "{1, 3}") ~expected:false;
  check_read_overlap (vector_reads "3" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "2" "{1, k}") ~expected:true;
  check_read_overlap (vector_reads "1" "{1}") ~expected:true;
  check_read_overlap (vector_reads "2" "{1}") ~expected:false;
  check_read_overlap (vector_reads "10" "{10, 1, 10}") ~expected:true;
  check_read_overlap (vector_reads "10" "{1, 9}") ~expected:false;
  check_read_overlap (vector_reads "k" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "2" "idx") ~expected:true

let%test_unit "Index overlap: Upfrom / All" =
  check_read_overlap (vector_reads "3:" ":") ~expected:true;
  check_read_overlap (vector_reads "1:" ":") ~expected:true;
  check_read_overlap (vector_reads "10:" ":") ~expected:true;
  check_read_overlap (vector_reads "k:" ":") ~expected:true

let%test_unit "Index overlap: Upfrom / Single" =
  check_read_overlap (vector_reads "3:" "2") ~expected:false;
  check_read_overlap (vector_reads "3:" "4") ~expected:true;
  check_read_overlap (vector_reads "3:" "3") ~expected:true;
  check_read_overlap (vector_reads "10:" "10") ~expected:true;
  check_read_overlap (vector_reads "10:" "1") ~expected:false;
  check_read_overlap (vector_reads "3:" "k") ~expected:true;
  check_read_overlap (vector_reads "k:" "3") ~expected:true;
  check_read_overlap (vector_reads "j:" "k") ~expected:true

let%test_unit "Index overlap: Upfrom / Upfrom" =
  check_read_overlap (vector_reads "2:" "3:") ~expected:true;
  check_read_overlap (vector_reads "1:" "1:") ~expected:true;
  check_read_overlap (vector_reads "3:" "3:") ~expected:true;
  check_read_overlap (vector_reads "1:" "10:") ~expected:true;
  check_read_overlap (vector_reads "10:" "10:") ~expected:true;
  check_read_overlap (vector_reads "k:" "j:") ~expected:true

let%test_unit "Index overlap: Upfrom / Between" =
  check_read_overlap (vector_reads "3:" "1:2") ~expected:false;
  check_read_overlap (vector_reads "3:" ":2") ~expected:false;
  check_read_overlap (vector_reads "3:" "3:4") ~expected:true;
  check_read_overlap (vector_reads "3:" "1:3") ~expected:true;
  check_read_overlap (vector_reads "3:" "4:5") ~expected:true;
  check_read_overlap (vector_reads "10:" "10:10") ~expected:true;
  check_read_overlap (vector_reads "3:" "4:3") ~expected:false;
  check_read_overlap (vector_reads "3:" "k:2") ~expected:false;
  check_read_overlap (vector_reads "k:" "1:2") ~expected:true;
  check_read_overlap (vector_reads "3:" "1:j") ~expected:true

let%test_unit "Index overlap: Upfrom / MultiIndex" =
  check_read_overlap (vector_reads "3:" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "3:" "{1, 2}") ~expected:false;
  check_read_overlap (vector_reads "3:" "{4, 5}") ~expected:true;
  check_read_overlap (vector_reads "10:" "{10, 1, 10}") ~expected:true;
  check_read_overlap (vector_reads "10:" "{1, 9}") ~expected:false;
  check_read_overlap (vector_reads "k:" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "3:" "idx") ~expected:true;
  check_read_overlap (vector_reads "3:" "{1, k}") ~expected:true

let%test_unit "Index overlap: Between / All" =
  check_read_overlap (vector_reads "1:2" ":") ~expected:true;
  check_read_overlap (vector_reads "3:2" ":") ~expected:false;
  check_read_overlap (vector_reads "1:10" ":") ~expected:true;
  check_read_overlap (vector_reads "5:5" ":") ~expected:true;
  check_read_overlap (vector_reads "10:1" ":") ~expected:false;
  check_read_overlap (vector_reads "k:j" ":") ~expected:true

let%test_unit "Index overlap: Between / Single" =
  check_read_overlap (vector_reads "1:2" "2") ~expected:true;
  check_read_overlap (vector_reads "1:2" "3") ~expected:false;
  check_read_overlap (vector_reads "1:k" "1") ~expected:true;
  check_read_overlap (vector_reads "2:4" "2") ~expected:true;
  check_read_overlap (vector_reads "2:4" "4") ~expected:true;
  check_read_overlap (vector_reads "2:4" "1") ~expected:false;
  check_read_overlap (vector_reads "3:3" "3") ~expected:true;
  check_read_overlap (vector_reads "4:3" "3") ~expected:false;
  check_read_overlap (vector_reads "2:4" "k") ~expected:true;
  check_read_overlap (vector_reads "2:k" "1") ~expected:false;
  check_read_overlap (vector_reads "k:9" "10") ~expected:false

let%test_unit "Index overlap: Between / Upfrom" =
  check_read_overlap (vector_reads "1:2" "3:") ~expected:false;
  check_read_overlap (vector_reads ":2" "3:") ~expected:false;
  check_read_overlap (vector_reads "3:4" "3:") ~expected:true;
  check_read_overlap (vector_reads "1:3" "3:") ~expected:true;
  check_read_overlap (vector_reads "4:5" "3:") ~expected:true;
  check_read_overlap (vector_reads "10:10" "10:") ~expected:true;
  check_read_overlap (vector_reads "4:3" "3:") ~expected:false;
  check_read_overlap (vector_reads "k:2" "3:") ~expected:false;
  check_read_overlap (vector_reads "1:2" "k:") ~expected:true;
  check_read_overlap (vector_reads "1:j" "3:") ~expected:true

let%test_unit "Index overlap: Between / Between" =
  check_read_overlap (vector_reads "1:2" "3:4") ~expected:false;
  check_read_overlap (vector_reads "1:2" "2:4") ~expected:true;
  check_read_overlap (vector_reads "1:k" "j:4") ~expected:true;
  check_read_overlap (vector_reads "k:2" "3:j") ~expected:false;
  check_read_overlap (vector_reads "3:2" "1:4") ~expected:false;
  check_read_overlap (vector_reads "2:4" "2:4") ~expected:true;
  check_read_overlap (vector_reads "1:10" "2:3") ~expected:true;
  check_read_overlap (vector_reads "1:3" "3:5") ~expected:true;
  check_read_overlap (vector_reads "5:5" "1:10") ~expected:true;
  check_read_overlap (vector_reads "10:10" "10:10") ~expected:true;
  check_read_overlap (vector_reads "3:2" "4:3") ~expected:false;
  check_read_overlap (vector_reads "5:k" "j:4") ~expected:false

let%test_unit "Index overlap: Between / MultiIndex" =
  check_read_overlap (vector_reads "2:2" "{1, 3}") ~expected:false;
  check_read_overlap (vector_reads "2:3" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "1:2" "idx") ~expected:true;
  check_read_overlap (vector_reads "2:4" "{2, 4}") ~expected:true;
  check_read_overlap (vector_reads "2:4" "{3}") ~expected:true;
  check_read_overlap (vector_reads "2:4" "{1, 5}") ~expected:false;
  check_read_overlap (vector_reads "2:4" "{4, 2, 4}") ~expected:true;
  check_read_overlap (vector_reads "3:2" "{2, 3}") ~expected:false;
  check_read_overlap (vector_reads "2:4" "{k}") ~expected:true;
  check_read_overlap (vector_reads "k:2" "{3, 4}") ~expected:false;
  check_read_overlap (vector_reads "3:j" "{1, 2}") ~expected:false

let%test_unit "Index overlap: MultiIndex / All" =
  check_read_overlap (vector_reads "{1, 3}" ":") ~expected:true;
  check_read_overlap (vector_reads "{1}" ":") ~expected:true;
  check_read_overlap (vector_reads "{10, 1, 10}" ":") ~expected:true;
  check_read_overlap (vector_reads "idx" ":") ~expected:true;
  check_read_overlap (vector_reads "{k, j}" ":") ~expected:true

let%test_unit "Index overlap: MultiIndex / Single" =
  check_read_overlap (vector_reads "{1, 3}" "2") ~expected:false;
  check_read_overlap (vector_reads "{1, 3}" "3") ~expected:true;
  check_read_overlap (vector_reads "{1, k}" "2") ~expected:true;
  check_read_overlap (vector_reads "{1}" "1") ~expected:true;
  check_read_overlap (vector_reads "{1}" "2") ~expected:false;
  check_read_overlap (vector_reads "{10, 1, 10}" "10") ~expected:true;
  check_read_overlap (vector_reads "{1, 9}" "10") ~expected:false;
  check_read_overlap (vector_reads "{1, 3}" "k") ~expected:true;
  check_read_overlap (vector_reads "idx" "2") ~expected:true

let%test_unit "Index overlap: MultiIndex / Upfrom" =
  check_read_overlap (vector_reads "{1, 3}" "3:") ~expected:true;
  check_read_overlap (vector_reads "{1, 2}" "3:") ~expected:false;
  check_read_overlap (vector_reads "{4, 5}" "3:") ~expected:true;
  check_read_overlap (vector_reads "{10, 1, 10}" "10:") ~expected:true;
  check_read_overlap (vector_reads "{1, 9}" "10:") ~expected:false;
  check_read_overlap (vector_reads "{1, 3}" "k:") ~expected:true;
  check_read_overlap (vector_reads "idx" "3:") ~expected:true;
  check_read_overlap (vector_reads "{1, k}" "3:") ~expected:true

let%test_unit "Index overlap: MultiIndex / Between" =
  check_read_overlap (vector_reads "{1, 3}" "2:2") ~expected:false;
  check_read_overlap (vector_reads "{1, 3}" "2:3") ~expected:true;
  check_read_overlap (vector_reads "idx" "1:2") ~expected:true;
  check_read_overlap (vector_reads "{2, 4}" "2:4") ~expected:true;
  check_read_overlap (vector_reads "{3}" "2:4") ~expected:true;
  check_read_overlap (vector_reads "{1, 5}" "2:4") ~expected:false;
  check_read_overlap (vector_reads "{4, 2, 4}" "2:4") ~expected:true;
  check_read_overlap (vector_reads "{2, 3}" "3:2") ~expected:false;
  check_read_overlap (vector_reads "{k}" "2:4") ~expected:true;
  check_read_overlap (vector_reads "{3, 4}" "k:2") ~expected:false;
  check_read_overlap (vector_reads "{1, 2}" "3:j") ~expected:false

let%test_unit "Index overlap: MultiIndex / MultiIndex" =
  check_read_overlap (vector_reads "{1, 3}" "{2, 4}") ~expected:false;
  check_read_overlap (vector_reads "{1, 3}" "{3, 4}") ~expected:true;
  check_read_overlap (vector_reads "{1, 1}" "{2, 2}") ~expected:false;
  check_read_overlap (vector_reads "idx" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "{1, 3}" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "{3, 1}" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "{1, 1}" "{1, 1}") ~expected:true;
  check_read_overlap (vector_reads "{1}" "{2}") ~expected:false;
  check_read_overlap (vector_reads "{1}" "{1}") ~expected:true;
  check_read_overlap (vector_reads "idx" "idx") ~expected:true;
  check_read_overlap (vector_reads "{k}" "{j}") ~expected:true;
  check_read_overlap (vector_reads "{1, k}" "{2, 3}") ~expected:true

let%test_unit "Different columns remain disjoint after All" =
  check_read_overlap (matrix_reads ":, 1" ":, 2") ~expected:false

let%test_unit "Overlapping row ranges share a column" =
  check_read_overlap (matrix_reads "1:2, 1" "2:3, 1") ~expected:true

let%test_unit "Different columns separate unknown row selections" =
  check_read_overlap (matrix_reads "idx, 1" "k:4, 2") ~expected:false

let%test_unit "Disjoint column ranges separate matching row arrays" =
  check_read_overlap (matrix_reads "{1, 3}, 1:2" "{1, 3}, 3:4") ~expected:false

let%test_unit "A whole column overlaps a whole row at their intersection" =
  check_read_overlap (matrix_reads ":, 1" "1, :") ~expected:true

let%test_unit "A whole row is disjoint from an element in another row" =
  check_read_overlap (matrix_reads "1" "2, 1") ~expected:false

let%test_unit "A whole row overlaps an element in that row" =
  check_read_overlap (matrix_reads "1" "1, 2") ~expected:true

let%test_unit "Nested slice: position one overlaps original element two" =
  check_read_overlap ~expected:true
    {|
      model {
        vector[4] v;
        print(v[2:4][1]);
        print(v[2]);
      }
    |}

let%test_unit "Nested slice: an element outside the base slice is disjoint" =
  check_read_overlap ~expected:false
    {|
      model {
        vector[4] v;
        print(v[2:4][1]);
        print(v[1]);
      }
    |}

let%test_unit "Nested slice: the retained base slice can overestimate overlap" =
  (* The first element of v[2:4] is v[2]. We record the whole slice as a read,
     so v[3] also shows up as a possible overlap. *)
  check_read_overlap ~expected:true
    {|
      model {
        vector[4] v;
        print(v[2:4][1]);
        print(v[3]);
      }
    |}

let check_index_pairs ~first ~second ~expected =
  List.iter first ~f:(fun a ->
      List.iter second ~f:(fun b ->
          check_overlap [Access.Subscript a] [Access.Subscript b] expected;
          check_overlap [Access.Subscript b] [Access.Subscript a] expected))

let%test_unit "Empty selections never overlap any index form" =
  let open Expr.Helpers in
  let int_array_meta = {Expr.Typed.Meta.empty with type_= UArray UInt} in
  let empty = Index.MultiIndex (internal_funapp FnMakeArray [] int_array_meta) in
  let idx = Index.MultiIndex {(variable "idx") with meta= int_array_meta} in
  check_index_pairs
    ~first:[empty; Between (int 3, int 2)]
    ~second:
      [ All; Single (int 2); Upfrom (int 2); Between (int 1, int 3)
      ; MultiIndex (array_expr [int 2]); idx; empty ]
    ~expected:false

let for_each_print_argument source ~f =
  let source = String.trim source in
  let lines = String.split_lines source |> Array.of_list in
  LabelMap.iter (statement_map source) ~f:(fun ~key:_ ~data:(pattern, loc) ->
      match pattern with
      | Stmt.Pattern.NRFunApp (CompilerInternal FnPrint, [arg]) ->
          let line = loc.Location_span.begin_loc.line_num in
          Fmt.pr "line %d: %s -> " line (String.trim lines.(line - 1));
          f arg
      | _ -> ())

let%expect_test "int_literal_value returns the integer written in the source" =
  for_each_print_argument
    {|data { int k; }
      model {
        print(42);
        print(k);
        print(2 + 3);
      }|}
    ~f:(fun arg ->
      match Expr.Helpers.int_literal_value arg with
      | Some i -> Fmt.pr "%d@." i
      | None -> Fmt.pr "no integer literal@.");
  [%expect
    {|
    line 3: print(42); -> 42
    line 4: print(k); -> no integer literal
    line 5: print(2 + 3); -> no integer literal
    |}]

let%expect_test "array_literal_elements returns the elements in source order" =
  for_each_print_argument
    {|data { int k; array[2] int idx; }
      model {
        print({3, k});
        print(idx);
      }|}
    ~f:(fun arg ->
      match Expr.Helpers.array_literal_elements arg with
      | Some elements ->
          Fmt.pr "elements [%a]@."
            (Fmt.list ~sep:(Fmt.any ", ") Expr.Typed.pp)
            elements
      | None -> Fmt.pr "no array literal@.");
  [%expect
    {|
    line 3: print({3, k}); -> elements [3, k]
    line 4: print(idx); -> no array literal
    |}]

let%expect_test
    "Index.is_single distinguishes a scalar index from a range or index array" =
  for_each_print_argument
    {|data { vector[4] v; array[1] int idx; }
      model {
        print(v[2]);
        print(v[:]);
        print(v[2:]);
        print(v[2:2]);
        print(v[idx]);
      }|}
    ~f:(fun arg ->
      match arg.pattern with
      | Indexed (_, [index]) ->
          Fmt.pr "single index: %b@." (Index.is_single index)
      | _ -> failwith "Expected an indexed expression");
  [%expect
    {|
    line 3: print(v[2]); -> single index: true
    line 4: print(v[:]); -> single index: false
    line 5: print(v[2:]); -> single index: false
    line 6: print(v[2:2]); -> single index: false
    line 7: print(v[idx]); -> single index: false
    |}]

let%expect_test
    "may_update_target distinguishes an _lp call from a target read or a plain \
     call" =
  for_each_print_argument
    {|functions {
        real f_lp(real x) { target += x; return x; }
        real f(real x) { return x; }
      }
      model {
        print(f_lp(1.0));
        print(target());
        print(f(1.0));
      }|}
    ~f:(fun arg ->
      match arg.pattern with
      | FunApp (kind, _) ->
          Fmt.pr "may update target: %b@." (Fun_kind.may_update_target kind)
      | _ -> failwith "Expected a function call");
  [%expect
    {|
    line 6: print(f_lp(1.0)); -> may update target: true
    line 7: print(target()); -> may update target: false
    line 8: print(f(1.0)); -> may update target: false
    |}]
