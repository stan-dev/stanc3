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

let variable_types statements =
  let rec collect_expr types (expr : Expr.Typed.t) =
    let types =
      match expr.pattern with
      | Var name -> String.Map.add types ~key:name ~data:expr.meta.type_
      | _ -> types in
    Expr.Pattern.fold collect_expr types expr.pattern in
  LabelMap.fold statements ~init:String.Map.empty
    ~f:(fun ~key:_ ~data:(pattern, _) types ->
      let types = Stmt.Pattern.fold collect_expr Fun.const types pattern in
      match pattern with
      | Decl {decl_id; decl_type; _} ->
          String.Map.add types ~key:decl_id ~data:(Type.to_unsized decl_type)
      | _ -> types)

let location_text types (accessed_location : Access.location) =
  let index_text index = Fmt.str "[%a]" (Index.pp pp_index_expr) index in
  (* Both t.1 and v[1] have a Single 1 in their path. The type tells us which
     syntax to print at each step. *)
  let rec path_text type_ path =
    match (type_, path) with
    | _, [] -> ""
    | UnsizedType.UTuple fields, Index.Single field :: rest ->
        let field = Expr.Helpers.int_literal_value field |> Option.get in
        Printf.sprintf ".%d%s" field
          (path_text (List.nth fields (field - 1)) rest)
    | UArray element, index :: rest -> index_text index ^ path_text element rest
    | UTuple _, _ -> failwith "Expected a literal tuple field in access path"
    | _, _ -> String.concat ~sep:"" (List.map path ~f:index_text) in
  match accessed_location.path with
  | [] -> accessed_location.variable
  | path ->
      accessed_location.variable
      ^ path_text (String.Map.find accessed_location.variable types) path

type statement_accesses =
  {statement: string; reads: string list; writes: string list}

let check_accesses ~statements:kind ~expected source =
  let statements = statement_map source in
  let types = variable_types statements in
  let lines = String.split_lines (String.trim source) |> Array.of_list in
  let paths locations =
    List.map locations ~f:(location_text types) |> List.sort ~cmp:String.compare
  in
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

let print_write_comparison variables ~previous_line ~next_line ~may_overlap
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
              List.filter_map (Access.collect_accesses pattern).writes
                ~f:(fun write ->
                  if List.mem write.Access.variable ~set:variables then
                    Some (pattern, write)
                  else None)
          | _ -> []) in
    match writes with
    | [write] -> write
    | _ ->
        Fmt.failwith "Expected one write to %s on line %d, found %d"
          (String.concat ~sep:", " variables)
          line (List.length writes) in
  if previous_line >= next_line then
    Fmt.failwith "Expected previous line %d to precede next line %d"
      previous_line next_line;
  let previous_statement, previous = write_on_line previous_line in
  let next_statement, next_write = write_on_line next_line in
  let actual_overlap =
    Access.Location.same_var previous next_write
    && Access.may_overlap previous.path next_write.path in
  let actual_overwritten =
    Access.is_overwritten written_vars previous next_write in
  Fmt.pr "previous (line %d): %a@.next (line %d): %a@." previous_line
    pp_statement previous_statement next_line pp_statement next_statement;
  Fmt.pr "may overlap: %b@.previous overwritten: %b@." actual_overlap
    actual_overwritten;
  let check name expected actual =
    if expected <> actual then
      Fmt.failwith "%s for lines %d and %d: expected %b, got %b" name
        previous_line next_line expected actual in
  check "may_overlap" may_overlap actual_overlap;
  check "previous_overwritten" previous_overwritten actual_overwritten

let%expect_test "Overwrite: different literal elements retain the earlier write"
    =
  print_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        v[1] = 1;
        v[2] = 2;
      }
    |};
  [%expect
    {|
    previous (line 3): v[1] = 1;
    next (line 4): v[2] = 2;
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: the same literal element replaces the earlier write"
    =
  print_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:true
    {|
      model {
        vector[4] v;
        v[1] = 1;
        v[1] = 2;
      }
    |};
  [%expect
    {|
    previous (line 3): v[1] = 1;
    next (line 4): v[1] = 2;
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: the same stable index replaces the earlier write" =
  print_write_comparison ["v"] ~previous_line:6 ~next_line:7 ~may_overlap:true
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
    |};
  [%expect
    {|
    previous (line 6): v[k] = 1;
    next (line 7): v[k] = 2;
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: different symbolic indices can alias" =
  print_write_comparison ["v"] ~previous_line:7 ~next_line:8 ~may_overlap:true
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
    |};
  [%expect
    {|
    previous (line 7): v[k] = 1;
    next (line 8): v[j] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: algebraic equivalence is not yet used" =
  print_write_comparison ["v"] ~previous_line:6 ~next_line:7 ~may_overlap:true
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
    |};
  [%expect
    {|
    previous (line 6): v[(k + k)] = 1;
    next (line 7): v[(2 * k)] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: stable indirect indices replace the earlier write" =
  print_write_comparison ["v"] ~previous_line:7 ~next_line:8 ~may_overlap:true
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
    |};
  [%expect
    {|
    previous (line 7): v[idx[k]] = 1;
    next (line 8): v[idx[k]] = 2;
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: nonlinear indices can alias a symbolic element" =
  print_write_comparison ["v"] ~previous_line:7 ~next_line:8 ~may_overlap:true
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
    |};
  [%expect
    {|
    previous (line 7): v[(k * k)] = 1;
    next (line 8): v[j] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: changing the scalar index retains the earlier write"
    =
  print_write_comparison ["v"] ~previous_line:4 ~next_line:6 ~may_overlap:true
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        int k = 1;
        v[k] = 1;
        k = 2;
        v[k] = 2;
      }
    |};
  [%expect
    {|
    previous (line 4): v[k] = 1;
    next (line 6): v[k] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: changing the index array retains the earlier write"
    =
  print_write_comparison ["v"] ~previous_line:4 ~next_line:6 ~may_overlap:true
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        array[1] int idx = {1};
        v[idx[1]] = 1;
        idx[1] = 2;
        v[idx[1]] = 2;
      }
    |};
  [%expect
    {|
    previous (line 4): v[idx[1]] = 1;
    next (line 6): v[idx[1]] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: different tuple fields remain distinct" =
  print_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        tuple(real, real) t;
        t.1 = 1;
        t.2 = 2;
      }
    |};
  [%expect
    {|
    previous (line 3): t.1 = 1;
    next (line 4): t.2 = 2;
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: an element write does not replace the whole vector"
    =
  print_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        v = rep_vector(1, 4);
        v[1] = 2;
      }
    |};
  [%expect
    {|
    previous (line 3): v = rep_vector(1, 4);
    next (line 4): v[1] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: a whole-vector write replaces an element" =
  print_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:true
    {|
      model {
        vector[4] v;
        v[1] = 1;
        v = rep_vector(2, 4);
      }
    |};
  [%expect
    {|
    previous (line 3): v[1] = 1;
    next (line 4): v = rep_vector(2, 4);
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: different variables remain distinct" =
  print_write_comparison ["v"; "w"] ~previous_line:4 ~next_line:5
    ~may_overlap:false ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        vector[4] w;
        v[1] = 1;
        w = rep_vector(2, 4);
      }
    |};
  [%expect
    {|
    previous (line 4): v[1] = 1;
    next (line 5): w = rep_vector(2, 4);
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: whole writes to different variables remain distinct"
    =
  print_write_comparison ["v"; "w"] ~previous_line:4 ~next_line:5
    ~may_overlap:false ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        vector[4] w;
        v = rep_vector(1, 4);
        w = rep_vector(2, 4);
      }
    |};
  [%expect
    {|
    previous (line 4): v = rep_vector(1, 4);
    next (line 5): w = rep_vector(2, 4);
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: disjoint slices retain the earlier write" =
  print_write_comparison ["v"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        vector[4] v;
        v[1:2] = rep_vector(1, 2);
        v[3:4] = rep_vector(2, 2);
      }
    |};
  [%expect
    {|
    previous (line 3): v[1:2] = rep_vector(1, 2);
    next (line 4): v[3:4] = rep_vector(2, 2);
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test
    "Overwrite: tuple fields in different array elements remain distinct" =
  print_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        array[2] tuple(real, real) t;
        t[1].2 = 1;
        t[2].1 = 2;
      }
    |};
  [%expect
    {|
    previous (line 3): t[1].2 = 1;
    next (line 4): t[2].1 = 2;
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test
    "Overwrite: tuple fields in the same array element remain distinct" =
  print_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:false
    ~previous_overwritten:false
    {|
      model {
        array[2] tuple(real, real) t;
        t[1].1 = 1;
        t[1].2 = 2;
      }
    |};
  [%expect
    {|
    previous (line 3): t[1].1 = 1;
    next (line 4): t[1].2 = 2;
    may overlap: false
    previous overwritten: false
    |}]

let%expect_test "Overwrite: the same tuple field replaces the earlier write" =
  print_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:true
    {|
      model {
        array[2] tuple(real, real) t;
        t[1].2 = 1;
        t[1].2 = 2;
      }
    |};
  [%expect
    {|
    previous (line 3): t[1].2 = 1;
    next (line 4): t[1].2 = 2;
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test
    "Overwrite: an element does not replace its enclosing tuple field" =
  print_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:false
    {|
      model {
        tuple(vector[4], real) t;
        t.1 = rep_vector(1, 4);
        t.1[2] = 2;
      }
    |};
  [%expect
    {|
    previous (line 3): t.1 = rep_vector(1, 4);
    next (line 4): t.1[2] = 2;
    may overlap: true
    previous overwritten: false
    |}]

let%expect_test "Overwrite: a tuple field replaces its contained element" =
  print_write_comparison ["t"] ~previous_line:3 ~next_line:4 ~may_overlap:true
    ~previous_overwritten:true
    {|
      model {
        tuple(vector[4], real) t;
        t.1[2] = 1;
        t.1 = rep_vector(2, 4);
      }
    |};
  [%expect
    {|
    previous (line 3): t.1[2] = 1;
    next (line 4): t.1 = rep_vector(2, 4);
    may overlap: true
    previous overwritten: true
    |}]

let%expect_test "Overwrite: repeated RNG calls can select different elements" =
  print_write_comparison ["v"] ~previous_line:4 ~next_line:5 ~may_overlap:true
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
    |};
  [%expect
    {|
    previous (line 4): v[categorical_rng(theta)] = 1;
    next (line 5): v[categorical_rng(theta)] = 2;
    may overlap: true
    previous overwritten: false
    |}]

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

let%expect_test "Index overlap: All / All" =
  check_read_overlap (vector_reads ":" ":") ~expected:true;
  [%expect
    {|
    PASS: v[:] / v[:]; may overlap: true
    PASS: v[:] / v[:]; may overlap: true
    |}]

let%expect_test "Index overlap: All / Single" =
  check_read_overlap (vector_reads ":" "2") ~expected:true;
  check_read_overlap (vector_reads ":" "1") ~expected:true;
  check_read_overlap (vector_reads ":" "10") ~expected:true;
  check_read_overlap (vector_reads ":" "k") ~expected:true;
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
  check_read_overlap (vector_reads ":" "3:") ~expected:true;
  check_read_overlap (vector_reads ":" "1:") ~expected:true;
  check_read_overlap (vector_reads ":" "10:") ~expected:true;
  check_read_overlap (vector_reads ":" "k:") ~expected:true;
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
  check_read_overlap (vector_reads ":" "1:2") ~expected:true;
  check_read_overlap (vector_reads ":" "3:2") ~expected:false;
  check_read_overlap (vector_reads ":" "1:10") ~expected:true;
  check_read_overlap (vector_reads ":" "5:5") ~expected:true;
  check_read_overlap (vector_reads ":" "10:1") ~expected:false;
  check_read_overlap (vector_reads ":" "k:j") ~expected:true;
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
  check_read_overlap (vector_reads ":" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads ":" "{1}") ~expected:true;
  check_read_overlap (vector_reads ":" "{10, 1, 10}") ~expected:true;
  check_read_overlap (vector_reads ":" "idx") ~expected:true;
  check_read_overlap (vector_reads ":" "{k, j}") ~expected:true;
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
  check_read_overlap (vector_reads "2" ":") ~expected:true;
  check_read_overlap (vector_reads "1" ":") ~expected:true;
  check_read_overlap (vector_reads "10" ":") ~expected:true;
  check_read_overlap (vector_reads "k" ":") ~expected:true;
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
  check_read_overlap (vector_reads "1" "1") ~expected:true;
  check_read_overlap (vector_reads "1" "2") ~expected:false;
  check_read_overlap (vector_reads "2 * k" "2 * k") ~expected:true;
  check_read_overlap (vector_reads "k" "j") ~expected:true;
  check_read_overlap (vector_reads "2 * k" "2 * k + 1") ~expected:false;
  check_read_overlap (vector_reads "10" "10") ~expected:true;
  check_read_overlap (vector_reads "1" "10") ~expected:false;
  check_read_overlap (vector_reads "1" "k") ~expected:true;
  check_read_overlap (vector_reads "k" "k") ~expected:true;
  check_read_overlap (vector_reads "k + 1 * j" "k + 1 * j") ~expected:true;
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
  check_read_overlap (vector_reads "2 * k" "2 * j + 1") ~expected:false;
  check_read_overlap (vector_reads "k * 2" "2 * j - 1") ~expected:false;
  check_read_overlap (vector_reads "-(2 * k)" "2 * j + 1") ~expected:false;
  check_read_overlap (vector_reads "2 * k + 1" "2 * j + 3") ~expected:true;
  check_read_overlap (vector_reads "2 * k" "2 * j") ~expected:true;
  check_read_overlap (vector_reads "k * j" "2") ~expected:true;
  check_read_overlap (vector_reads "k * j" "3") ~expected:true;
  check_read_overlap (vector_reads "k + 1" "2 * j") ~expected:true;
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
  check_read_overlap (vector_reads "2" "3:") ~expected:false;
  check_read_overlap (vector_reads "4" "3:") ~expected:true;
  check_read_overlap (vector_reads "3" "3:") ~expected:true;
  check_read_overlap (vector_reads "10" "10:") ~expected:true;
  check_read_overlap (vector_reads "1" "10:") ~expected:false;
  check_read_overlap (vector_reads "k" "3:") ~expected:true;
  check_read_overlap (vector_reads "3" "k:") ~expected:true;
  check_read_overlap (vector_reads "k" "j:") ~expected:true;
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
  check_read_overlap (vector_reads "10" "k:9") ~expected:false;
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
  check_read_overlap (vector_reads "2" "{1, 3}") ~expected:false;
  check_read_overlap (vector_reads "3" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "2" "{1, k}") ~expected:true;
  check_read_overlap (vector_reads "1" "{1}") ~expected:true;
  check_read_overlap (vector_reads "2" "{1}") ~expected:false;
  check_read_overlap (vector_reads "10" "{10, 1, 10}") ~expected:true;
  check_read_overlap (vector_reads "10" "{1, 9}") ~expected:false;
  check_read_overlap (vector_reads "k" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "2" "idx") ~expected:true;
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
  check_read_overlap (vector_reads "3:" ":") ~expected:true;
  check_read_overlap (vector_reads "1:" ":") ~expected:true;
  check_read_overlap (vector_reads "10:" ":") ~expected:true;
  check_read_overlap (vector_reads "k:" ":") ~expected:true;
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
  check_read_overlap (vector_reads "3:" "2") ~expected:false;
  check_read_overlap (vector_reads "3:" "4") ~expected:true;
  check_read_overlap (vector_reads "3:" "3") ~expected:true;
  check_read_overlap (vector_reads "10:" "10") ~expected:true;
  check_read_overlap (vector_reads "10:" "1") ~expected:false;
  check_read_overlap (vector_reads "3:" "k") ~expected:true;
  check_read_overlap (vector_reads "k:" "3") ~expected:true;
  check_read_overlap (vector_reads "j:" "k") ~expected:true;
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
  check_read_overlap (vector_reads "2:" "3:") ~expected:true;
  check_read_overlap (vector_reads "1:" "1:") ~expected:true;
  check_read_overlap (vector_reads "3:" "3:") ~expected:true;
  check_read_overlap (vector_reads "1:" "10:") ~expected:true;
  check_read_overlap (vector_reads "10:" "10:") ~expected:true;
  check_read_overlap (vector_reads "k:" "j:") ~expected:true;
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
  check_read_overlap (vector_reads "3:" "1:2") ~expected:false;
  check_read_overlap (vector_reads "3:" ":2") ~expected:false;
  check_read_overlap (vector_reads "3:" "3:4") ~expected:true;
  check_read_overlap (vector_reads "3:" "1:3") ~expected:true;
  check_read_overlap (vector_reads "3:" "4:5") ~expected:true;
  check_read_overlap (vector_reads "10:" "10:10") ~expected:true;
  check_read_overlap (vector_reads "3:" "4:3") ~expected:false;
  check_read_overlap (vector_reads "3:" "k:2") ~expected:false;
  check_read_overlap (vector_reads "k:" "1:2") ~expected:true;
  check_read_overlap (vector_reads "3:" "1:j") ~expected:true;
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
  check_read_overlap (vector_reads "3:" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "3:" "{1, 2}") ~expected:false;
  check_read_overlap (vector_reads "3:" "{4, 5}") ~expected:true;
  check_read_overlap (vector_reads "10:" "{10, 1, 10}") ~expected:true;
  check_read_overlap (vector_reads "10:" "{1, 9}") ~expected:false;
  check_read_overlap (vector_reads "k:" "{1, 3}") ~expected:true;
  check_read_overlap (vector_reads "3:" "idx") ~expected:true;
  check_read_overlap (vector_reads "3:" "{1, k}") ~expected:true;
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
  check_read_overlap (vector_reads "1:2" ":") ~expected:true;
  check_read_overlap (vector_reads "3:2" ":") ~expected:false;
  check_read_overlap (vector_reads "1:10" ":") ~expected:true;
  check_read_overlap (vector_reads "5:5" ":") ~expected:true;
  check_read_overlap (vector_reads "10:1" ":") ~expected:false;
  check_read_overlap (vector_reads "k:j" ":") ~expected:true;
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
  check_read_overlap (vector_reads "k:9" "10") ~expected:false;
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
  check_read_overlap (vector_reads "1:2" "3:") ~expected:false;
  check_read_overlap (vector_reads ":2" "3:") ~expected:false;
  check_read_overlap (vector_reads "3:4" "3:") ~expected:true;
  check_read_overlap (vector_reads "1:3" "3:") ~expected:true;
  check_read_overlap (vector_reads "4:5" "3:") ~expected:true;
  check_read_overlap (vector_reads "10:10" "10:") ~expected:true;
  check_read_overlap (vector_reads "4:3" "3:") ~expected:false;
  check_read_overlap (vector_reads "k:2" "3:") ~expected:false;
  check_read_overlap (vector_reads "1:2" "k:") ~expected:true;
  check_read_overlap (vector_reads "1:j" "3:") ~expected:true;
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
  check_read_overlap (vector_reads "5:k" "j:4") ~expected:false;
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
  check_read_overlap (vector_reads "3:j" "{1, 2}") ~expected:false;
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
  check_read_overlap (vector_reads "{1, 3}" ":") ~expected:true;
  check_read_overlap (vector_reads "{1}" ":") ~expected:true;
  check_read_overlap (vector_reads "{10, 1, 10}" ":") ~expected:true;
  check_read_overlap (vector_reads "idx" ":") ~expected:true;
  check_read_overlap (vector_reads "{k, j}" ":") ~expected:true;
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
  check_read_overlap (vector_reads "{1, 3}" "2") ~expected:false;
  check_read_overlap (vector_reads "{1, 3}" "3") ~expected:true;
  check_read_overlap (vector_reads "{1, k}" "2") ~expected:true;
  check_read_overlap (vector_reads "{1}" "1") ~expected:true;
  check_read_overlap (vector_reads "{1}" "2") ~expected:false;
  check_read_overlap (vector_reads "{10, 1, 10}" "10") ~expected:true;
  check_read_overlap (vector_reads "{1, 9}" "10") ~expected:false;
  check_read_overlap (vector_reads "{1, 3}" "k") ~expected:true;
  check_read_overlap (vector_reads "idx" "2") ~expected:true;
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
  check_read_overlap (vector_reads "{1, 3}" "3:") ~expected:true;
  check_read_overlap (vector_reads "{1, 2}" "3:") ~expected:false;
  check_read_overlap (vector_reads "{4, 5}" "3:") ~expected:true;
  check_read_overlap (vector_reads "{10, 1, 10}" "10:") ~expected:true;
  check_read_overlap (vector_reads "{1, 9}" "10:") ~expected:false;
  check_read_overlap (vector_reads "{1, 3}" "k:") ~expected:true;
  check_read_overlap (vector_reads "idx" "3:") ~expected:true;
  check_read_overlap (vector_reads "{1, k}" "3:") ~expected:true;
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
  check_read_overlap (vector_reads "{1, 2}" "3:j") ~expected:false;
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
  check_read_overlap (vector_reads "{1, k}" "{2, 3}") ~expected:true;
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
  check_read_overlap (matrix_reads ":, 1" ":, 2") ~expected:false;
  [%expect
    {|
    PASS: v[:, 1] / v[:, 2]; may overlap: false
    PASS: v[:, 2] / v[:, 1]; may overlap: false
    |}]

let%expect_test "Overlapping row ranges share a column" =
  check_read_overlap (matrix_reads "1:2, 1" "2:3, 1") ~expected:true;
  [%expect
    {|
    PASS: v[1:2, 1] / v[2:3, 1]; may overlap: true
    PASS: v[2:3, 1] / v[1:2, 1]; may overlap: true
    |}]

let%expect_test "Different columns separate unknown row selections" =
  check_read_overlap (matrix_reads "idx, 1" "k:4, 2") ~expected:false;
  [%expect
    {|
    PASS: v[idx, 1] / v[k:4, 2]; may overlap: false
    PASS: v[k:4, 2] / v[idx, 1]; may overlap: false
    |}]

let%expect_test "Disjoint column ranges separate matching row arrays" =
  check_read_overlap (matrix_reads "{1, 3}, 1:2" "{1, 3}, 3:4") ~expected:false;
  [%expect
    {|
    PASS: v[{1, 3}, 1:2] / v[{1, 3}, 3:4]; may overlap: false
    PASS: v[{1, 3}, 3:4] / v[{1, 3}, 1:2]; may overlap: false
    |}]

let%expect_test "A whole column overlaps a whole row at their intersection" =
  check_read_overlap (matrix_reads ":, 1" "1, :") ~expected:true;
  [%expect
    {|
    PASS: v[:, 1] / v[1, :]; may overlap: true
    PASS: v[1, :] / v[:, 1]; may overlap: true
    |}]

let%expect_test "Nested slice: position one overlaps original element two" =
  check_read_overlap ~expected:true
    {|
      model {
        vector[4] v;
        print(v[2:4][1]);
        print(v[2]);
      }
    |};
  [%expect
    {|
    PASS: v[2:4] / v[2]; may overlap: true
    PASS: v[2] / v[2:4]; may overlap: true
    |}]

let%expect_test "Nested slice: an element outside the base slice is disjoint" =
  check_read_overlap ~expected:false
    {|
      model {
        vector[4] v;
        print(v[2:4][1]);
        print(v[1]);
      }
    |};
  [%expect
    {|
    PASS: v[2:4] / v[1]; may overlap: false
    PASS: v[1] / v[2:4]; may overlap: false
    |}]

let%expect_test "Nested slice: the retained base slice can overestimate overlap"
    =
  (* The first element of v[2:4] is v[2]. We record the whole slice as a read,
     so v[3] also shows up as a possible overlap. *)
  check_read_overlap ~expected:true
    {|
      model {
        vector[4] v;
        print(v[2:4][1]);
        print(v[3]);
      }
    |};
  [%expect
    {|
    PASS: v[2:4] / v[3]; may overlap: true
    PASS: v[3] / v[2:4]; may overlap: true
    |}]

let check_index_pairs ~first ~second ~expected =
  List.iter first ~f:(fun a ->
      List.iter second ~f:(fun b ->
          check_overlap [a] [b] expected;
          check_overlap [b] [a] expected))

let%expect_test "Empty selections never overlap any index form" =
  let open Expr.Helpers in
  let int_array_meta = {Expr.Typed.Meta.empty with type_= UArray UInt} in
  let empty = Index.MultiIndex (internal_funapp FnMakeArray [] int_array_meta) in
  let idx = Index.MultiIndex {(variable "idx") with meta= int_array_meta} in
  check_index_pairs
    ~first:[empty; Between (int 3, int 2)]
    ~second:
      [ All; Single (int 2); Upfrom (int 2); Between (int 1, int 3)
      ; MultiIndex (array_expr [int 2]); idx; empty ]
    ~expected:false;
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
