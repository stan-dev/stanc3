open Std
open Std.Sexp_conv
open Analysis_and_optimization.Dependence_analysis
open Middle
open Analysis_and_optimization.Dataflow_types

let check_dependency_graph ~source ~outer_dependencies ~dependencies_by_line =
  let mir = Test_utils.mir_of_string source in
  let graph = log_prob_dependency_graph mir in
  let statements =
    Analysis_and_optimization.Dataflow_utils.build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta.Location_span.begin_loc.line_num)
      Stmt.{meta= Location_span.empty; pattern= SList mir.log_prob} in
  let root, model =
    match LabelMap.min_binding statements with
    | root, (Stmt.Pattern.SList [model], _) -> (root, model)
    | _ -> failwith "Expected a single model block" in
  (* The outer statement list has no source line. The model block starts on line
     1 in this fixture, but MIR gives it the location of its first child. *)
  let source_line id =
    if id = root then None
    else if id = model then Some 1
    else Some (snd (LabelMap.find id statements)) in
  let actual =
    LabelMap.bindings graph
    |> List.map ~f:(fun (id, dependencies) ->
        (source_line id, List.map (Set.Poly.to_list dependencies) ~f:source_line))
  in
  let lines = String.split_lines source |> Array.of_list in
  let line_number = function None -> "-" | Some n -> Int.to_string n in
  let print rows =
    Fmt.pr "%4s  %-32s  %s@." "Line" "Stan source" "Depends on lines";
    List.iter rows ~f:(fun (line, dependencies) ->
        let code =
          match line with
          | None -> "<outer statement list>"
          | Some n -> lines.(n - 1) in
        let refs =
          match dependencies with
          | [] -> "none"
          | ds -> List.map ds ~f:line_number |> String.concat ~sep:", " in
        Fmt.pr "%4s  %-32s  %s@." (line_number line) code refs) in
  print actual;
  let expected =
    (None, List.map outer_dependencies ~f:Option.some)
    :: List.map dependencies_by_line ~f:(fun (line, deps) ->
        (Some line, List.map deps ~f:Option.some)) in
  if actual <> expected then (
    print_endline "Expected:";
    print expected;
    failwith "Dependency graph differs from the expected graph")

let loop_with_break_and_continue =
  {|model {
  int i;
  i = 0;
  if (i < 0)
  {
    print(i);
  } else {
    for (j in 1:10)
    {
      if (j > 9)
      {
        break;
      }
      if (j > 8 && i < -1)
      {
        continue;
      }
      if (j > 5)
      {
        continue;
      } else {
        print("Badger", i + j);
      }
      print("Fin");
    }
  }
}|}

let%expect_test "Transitive dependencies through a loop with break and continue"
    =
  check_dependency_graph ~source:loop_with_break_and_continue
    ~outer_dependencies:[]
    ~dependencies_by_line:
      [ (1, []); (2, []); (3, []); (4, [3]); (5, [3; 4]); (6, [3; 4])
      ; (7, [3; 4]); (8, [3; 4; 10; 12]); (9, [3; 4; 8; 10; 12])
      ; (10, [3; 4; 8; 12]); (11, [3; 4; 8; 10; 12]); (12, [3; 4; 8; 10])
      ; (14, [3; 4; 8; 10; 12]); (15, [3; 4; 8; 10; 12; 14])
      ; (16, [3; 4; 8; 10; 12; 14]); (18, [3; 4; 8; 10; 12; 14; 16])
      ; (19, [3; 4; 8; 10; 12; 14; 16; 18]); (20, [3; 4; 8; 10; 12; 14; 16; 18])
      ; (21, [3; 4; 8; 10; 12; 14; 16; 18]); (22, [3; 4; 8; 10; 12; 14; 16; 18])
      ; (24, [3; 4; 8; 10; 12; 14; 16; 18; 20]) ];
  [%expect
    {|
    Line  Stan source                       Depends on lines
       -  <outer statement list>            none
       1  model {                           none
       2    int i;                          none
       3    i = 0;                          none
       4    if (i < 0)                      3
       5    {                               3, 4
       6      print(i);                     3, 4
       7    } else {                        3, 4
       8      for (j in 1:10)               3, 4, 10, 12
       9      {                             3, 4, 8, 10, 12
      10        if (j > 9)                  3, 4, 8, 12
      11        {                           3, 4, 8, 10, 12
      12          break;                    3, 4, 8, 10
      14        if (j > 8 && i < -1)        3, 4, 8, 10, 12
      15        {                           3, 4, 8, 10, 12, 14
      16          continue;                 3, 4, 8, 10, 12, 14
      18        if (j > 5)                  3, 4, 8, 10, 12, 14, 16
      19        {                           3, 4, 8, 10, 12, 14, 16, 18
      20          continue;                 3, 4, 8, 10, 12, 14, 16, 18
      21        } else {                    3, 4, 8, 10, 12, 14, 16, 18
      22          print("Badger", i + j);   3, 4, 8, 10, 12, 14, 16, 18
      24        print("Fin");               3, 4, 8, 10, 12, 14, 16, 18, 20
    |}]

let example1_program = Test_utils.mir_of_string loop_with_break_and_continue

let%expect_test "Variable dependency example" =
  let deps =
    node_vars_dependencies
      (log_prob_build_dep_info_map example1_program)
      (Set.Poly.singleton (VVar "j"))
      17 in
  print_s [%sexp (deps : label Set.Poly.t)];
  [%expect {|
      (4 5 9 11 13 14 16)
    |}]

let uninitialized_var_example =
  Test_utils.mir_of_string
    {|
        functions {
          int f(int y) {
            int x;
            if (y > 2)
              return y + 24;
            return y + 2;
          }
        }
        data {
          real w;
        }
        transformed data {
          real wu;
          print(wu);
          print(w);
          wu = w;
        }
        parameters {
          real x;
        }
        model
        {
          int i;
          int z = 0;
          print(i);
          print(z);
          print(x);
          if (z == 1) {
            i = 1;
          } else {}
          print(i);
          if (z == 2) {
            i = 1;
          } else {
            i = 2;
          }
          print(i);
        }
        generated quantities {
          int k;
          print(k);
        }
      |}

let%expect_test "Uninitialized variables example" =
  let deps = mir_uninitialized_variables uninitialized_var_example in
  print_s [%sexp (deps : (Location_span.t * string) Set.Poly.t)];
  [%expect
    {|
      ((((begin_loc
          ((filename string) (line_num 15) (col_num 16) (byte_num 270)
           (included_from ())))
         (end_loc
          ((filename string) (line_num 15) (col_num 18) (byte_num 272)
           (included_from ()))))
        wu)
       (((begin_loc
          ((filename string) (line_num 26) (col_num 16) (byte_num 450)
           (included_from ())))
         (end_loc
          ((filename string) (line_num 26) (col_num 17) (byte_num 451)
           (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 32) (col_num 16) (byte_num 573)
           (included_from ())))
         (end_loc
          ((filename string) (line_num 32) (col_num 17) (byte_num 574)
           (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 42) (col_num 16) (byte_num 764)
           (included_from ())))
         (end_loc
          ((filename string) (line_num 42) (col_num 17) (byte_num 765)
           (included_from ()))))
        k))
    |}]

let source_lines source =
  String.split_lines source |> List.map ~f:String.trim |> Array.of_list

let check_lines ~what ~expected actual =
  if actual <> expected then
    let pp = Fmt.list ~sep:(Fmt.any ", ") Fmt.int in
    Fmt.failwith "%s: expected lines [%a], got [%a]" what pp expected pp actual

let check_read_writers ~block ~variable ~read_line ~expected_writer_lines source
    =
  let source = String.trim source in
  let lines = source_lines source in
  let mir = Test_utils.mir_of_string source in
  let statements =
    match block with
    | `Model -> log_prob_build_dep_info_map mir
    | `Function name ->
        build_dep_info_map mir (Test_utils.function_body_exn mir name) in
  let reads =
    LabelMap.bindings statements
    |> List.concat_map ~f:(function
      | _, (_, info) when info.meta.begin_loc.line_num = read_line ->
          List.filter_map info.accesses.reads ~f:(fun read ->
              if
                String.equal read.Analysis_and_optimization.Dependence.variable
                  variable
              then Some (info.reaching_defn_entry, read)
              else None)
      | _ -> []) in
  match reads with
  | [(definitions, read)] ->
      Fmt.pr "line %d: %s@.Read: %s%a@.Possible writers of %s:@." read_line
        lines.(read_line - 1)
        read.variable
        (Analysis_and_optimization.Dependence.pp_access_path Expr.Typed.pp)
        read.path variable;
      let writer_lines =
        reaching_defn_lookup definitions read
        |> Set.Poly.to_list
        |> List.map ~f:(fun id ->
            (snd (LabelMap.find id statements)).meta.begin_loc.line_num)
        |> List.sort ~cmp:Int.compare in
      List.iter writer_lines ~f:(fun line ->
          Fmt.pr "  line %d: %s@." line lines.(line - 1));
      check_lines
        ~what:(Fmt.str "Writers of %s at line %d" variable read_line)
        ~expected:expected_writer_lines writer_lines
  | _ ->
      Fmt.failwith "Expected one read of %s on line %d, found %d in:\n%s"
        variable read_line (List.length reads) source

let%expect_test "The for header supplies j to print(j)" =
  check_read_writers ~block:`Model ~variable:"j" ~read_line:3
    ~expected_writer_lines:[2]
    {|
      model {
        for (j in 1:10) {
          print(j);
        }
      }
    |};
  [%expect
    {|
    line 3: print(j);
    Read: j
    Possible writers of j:
      line 2: for (j in 1:10) {
    |}]

let%expect_test "A continue can carry x = 1 past the later write of x = 2" =
  check_read_writers ~block:`Model ~variable:"x" ~read_line:4
    ~expected_writer_lines:[2; 5; 7]
    {|
      model {
        int x = 0;
        for (j in 1:3) {
          print(x);
          x = 1;
          if (j == 1) continue;
          x = 2;
        }
      }
    |};
  [%expect
    {|
    line 4: print(x);
    Read: x
    Possible writers of x:
      line 2: int x = 0;
      line 5: x = 1;
      line 7: x = 2;
    |}]

let%expect_test
    "Random subscripts: the return can read the initial vector or either \
     element write" =
  check_read_writers ~block:(`Function "pick_rng") ~variable:"v" ~read_line:7
    ~expected_writer_lines:[4; 5; 6]
    {|
      functions {
        real pick_rng(vector theta) {
          vector[num_elements(theta) + 1] v;
          v = rep_vector(0, num_elements(theta) + 1);
          v[categorical_rng(theta)] = 1;
          v[categorical_rng(theta)] = 2;
          return v[categorical_rng(theta) + 1];
        }
      }
      model {}
    |};
  [%expect
    {|
    line 7: return v[categorical_rng(theta) + 1];
    Read: v[(categorical_rng(theta) + 1)]
    Possible writers of v:
      line 4: v = rep_vector(0, num_elements(theta) + 1);
      line 5: v[categorical_rng(theta)] = 1;
      line 6: v[categorical_rng(theta)] = 2;
    |}]

let check_assignment_writers ~at_line ~entry ~exit source =
  let source = String.trim source in
  let lines = source_lines source in
  let statements =
    log_prob_build_dep_info_map (Test_utils.mir_of_string source) in
  let assignments =
    LabelMap.bindings statements
    |> List.filter_map ~f:(function
      | _, (Stmt.Pattern.Assignment _, info)
        when info.meta.begin_loc.line_num = at_line ->
          Some info
      | _ -> None) in
  let check phase expected definitions =
    let actual =
      Set.Poly.filter_map definitions ~f:(fun (location, writer) ->
          Option.map writer ~f:(fun id ->
              let _, info = LabelMap.find id statements in
              ( location.Analysis_and_optimization.Dependence.variable
              , info.meta.begin_loc.line_num )))
      |> Set.Poly.to_list in
    let pp =
      Fmt.list ~sep:(Fmt.any ", ") (fun ppf (variable, line) ->
          Fmt.pf ppf "%s from line %d" variable line) in
    Fmt.pr "%s: [%a]@." phase pp actual;
    if actual <> expected then
      Fmt.failwith "%s at line %d: expected [%a], got [%a]" phase at_line pp
        expected pp actual in
  match assignments with
  | [info] ->
      Fmt.pr "line %d: %s@." at_line lines.(at_line - 1);
      check "entry" entry info.reaching_defn_entry;
      check "exit" exit info.reaching_defn_exit
  | _ ->
      Fmt.failwith "Expected one assignment on line %d, found %d" at_line
        (List.length assignments)

let%expect_test
    "Reassigning i replaces its writer and preserves the writer of j" =
  check_assignment_writers ~at_line:6
    ~entry:[("i", 4); ("j", 5)]
    ~exit:[("i", 6); ("j", 5)]
    {|
      model {
        int i;
        int j;
        i = 0;
        j = 1;
        i = j;
      }
    |};
  [%expect
    {|
    line 6: i = j;
    entry: [i from line 4, j from line 5]
    exit: [i from line 6, j from line 5]
    |}]
