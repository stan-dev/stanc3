open Std
open Std.Sexp_conv
open Analysis_and_optimization.Dependence_analysis
open Middle
open Analysis_and_optimization.Dataflow_types

let example1_program =
  Test_utils.mir_of_string
    {|
        model
        {                                // 1
          int i                          // 2: 3
              = 0;                       //    4
          if (i < 0)                     // 5
          {                              // 6
            print(i);                    // 7
          } else
          {                              // 8
            for (j in 1:10)              // 9
            {                            // 10
              if (j > 9)                 // 11
              {                          // 12
                break;                   // 13
              }
              if (j > 8 && i < -1)       // 14
              {                          // 15
                continue;                // 16
              }
              if (j > 5)                 // 17
              {                          // 18
                continue;                // 19
              } else
              {                          // 20
                print("Badger", i + j);  // 21
              }
              print("Fin");              // 22
            }
          }
        }
      |}

let%expect_test "Dependency graph example" =
  let deps = log_prob_dependency_graph example1_program in
  print_s [%sexp (deps : label Set.Poly.t LabelMap.t)];
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 (4 5)) (7 (4 5)) (8 (4 5))
       (9 (4 5 11 13)) (10 (4 5 9 11 13)) (11 (4 5 9 13)) (12 (4 5 9 11 13))
       (13 (4 5 9 11)) (14 (4 5 9 11 13)) (15 (4 5 9 11 13 14))
       (16 (4 5 9 11 13 14)) (17 (4 5 9 11 13 14 16)) (18 (4 5 9 11 13 14 16 17))
       (19 (4 5 9 11 13 14 16 17)) (20 (4 5 9 11 13 14 16 17))
       (21 (4 5 9 11 13 14 16 17)) (22 (4 5 9 11 13 14 16 17 19)))
    |}]

let%expect_test "Reaching defns example" =
  let deps =
    LabelMap.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        ( reaching_defn_lookup x.reaching_defn_entry {variable= "j"; path= []}
        , reaching_defn_lookup x.reaching_defn_exit {variable= "j"; path= []} ))
  in
  print_s [%sexp (deps : (label Set.Poly.t * label Set.Poly.t) LabelMap.t)];
  [%expect
    {|
      ((1 (() ())) (2 ((9) (9))) (3 (() ())) (4 (() ())) (5 (() ())) (6 (() ()))
       (7 (() ())) (8 ((9) (9))) (9 ((9) (9))) (10 ((9) (9))) (11 ((9) (9)))
       (12 ((9) (9))) (13 ((9) (9))) (14 ((9) (9))) (15 ((9) (9))) (16 ((9) (9)))
       (17 ((9) (9))) (18 ((9) (9))) (19 ((9) (9))) (20 ((9) (9))) (21 ((9) (9)))
       (22 ((9) (9))))
    |}]

let%expect_test "Reaching defns example" =
  let deps =
    LabelMap.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        let variable_labels definitions =
          Set.Poly.filter_map definitions ~f:(fun (location, label) ->
              Option.map label ~f:(fun id ->
                  ( VVar location.Analysis_and_optimization.Dependence.variable
                  , id ))) in
        ( variable_labels x.reaching_defn_entry
        , variable_labels x.reaching_defn_exit )) in
  print_s
    [%sexp
      (deps : (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) LabelMap.t)];
  [%expect
    {|
      ((1 (() ())) (2 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (3 (() (((VVar i) 3)))) (4 ((((VVar i) 3)) (((VVar i) 4))))
       (5 ((((VVar i) 4)) (((VVar i) 4)))) (6 ((((VVar i) 4)) (((VVar i) 4))))
       (7 ((((VVar i) 4)) (((VVar i) 4))))
       (8 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (9 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (10 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (11 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (12 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (13 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (14 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (15 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (16 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (17 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (18 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (19 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (20 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (21 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (22 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9)))))
    |}]

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

let assert_parameter_dependencies body expected =
  let mir =
    Test_utils.mir_of_string
      ("data { int k; } parameters { real a; } model { vector[10] v = \
        rep_vector(0, 10); " ^ body ^ " }") in
  let prefix = "A control flow statement depends on parameter(s): " in
  let actual =
    Analysis_and_optimization.Pedantic_analysis.warn_pedantic mir
    |> List.filter_map ~f:(fun (_, message) ->
        if String.starts_with ~prefix message then Some message else None) in
  let expected =
    List.map expected ~f:(fun names ->
        prefix ^ String.concat ~sep:", " names ^ ".") in
  if actual <> expected then
    failwith ("Unexpected parameter dependencies for: " ^ body)

let%expect_test "Indexed reads exclude distinct literal writes" =
  assert_parameter_dependencies "v[1] = a; if (v[2] > 0) target += 1;" [];
  assert_parameter_dependencies "v[1] = a; if (v[1] > 0) target += 1;" [["a"]];
  [%expect {| |}]

let%expect_test "Indexed overwrites require a stable address" =
  assert_parameter_dependencies "v[k] = a; v[k] = 0; if (v[k] > 0) target += 1;"
    [];
  assert_parameter_dependencies
    "int i = 1; v[i] = a; i = 2; v[i] = 0; if (v[1] > 0) target += 1;" [["a"]];
  assert_parameter_dependencies
    "array[2] int idx = {1, 2}; v[idx[1]] = a; idx[1] = 2; v[idx[1]] = 0; if \
     (v[1] > 0) target += 1;"
    [["a"]];
  [%expect {| |}]

let%expect_test
    "Loop bounds and overwrites remove false dependences but retain carried \
     flow" =
  assert_parameter_dependencies
    "for (n in 1:5) { v[n + 5] = a; if (v[n] > 0) target += 1; }" [];
  assert_parameter_dependencies
    "for (n in 2:5) { if (v[n - 1] > 0) target += 1; v[n] = a; }" [["a"]];
  assert_parameter_dependencies
    "for (n in 1:5) { v[n] = a; v[n] = 0; if (v[n] > 0) target += 1; }" [];
  [%expect {| |}]

let%expect_test "Conditional writes and slices preserve possible dependencies" =
  assert_parameter_dependencies
    "v[1] = a; if (k > 0) v[1] = 0; if (v[1] > 0) target += 1;" [["a"]];
  assert_parameter_dependencies "v[2] = a; if (v[2:3][1] > 0) target += 1;"
    [["a"]];
  assert_parameter_dependencies
    "v[1] = a; v[1:2] = rep_vector(0, 2); if (v[1] > 0) target += 1;" [["a"]];
  [%expect {| |}]

let%expect_test "Tuple fields have separate dependencies" =
  assert_parameter_dependencies
    "tuple(real, real) t; t.1 = a; t.2 = 0; if (t.2 > 0) target += 1;" [];
  assert_parameter_dependencies
    "tuple(real, real) t; t.1 = a; t.2 = 0; if (t.1 > 0) target += 1;" [["a"]];
  [%expect {| |}]

let%expect_test
    "Random subscripts retain both writes and the initial definition" =
  let mir =
    Test_utils.mir_of_string
      {|functions {
        real pick_rng(vector theta) {
          vector[4] v = rep_vector(0, 4);
          v[categorical_rng(theta)] = 1;
          v[categorical_rng(theta)] = 2;
          return v[categorical_rng(theta) + 1];
        }
      } model {}|}
  in
  let body =
    Option.get
      (List.find_map mir.functions_block ~f:(fun definition ->
           definition.Program.fdbody)) in
  let statements = build_dep_info_map mir body in
  LabelMap.iter statements ~f:(fun ~key:_ ~data:(pattern, info) ->
      match pattern with
      | Stmt.Pattern.Return _ ->
          let writers =
            reaching_defn_lookup info.reaching_defn_entry
              {variable= "v"; path= []} in
          print_s [%sexp (writers : label Set.Poly.t)]
      | _ -> ());
  [%expect {| (3 4 5) |}]

let%expect_test "Equivalent stable subscripts overwrite without syntax equality"
    =
  assert_parameter_dependencies
    "v[k + k] = a; v[2 * k] = 0; if (v[2 * k] > 0) target += 1;" [];
  assert_parameter_dependencies
    "v[k + k] = a; if (k > 0) v[2 * k] = 0; if (v[2 * k] > 0) target += 1;"
    [["a"]];
  [%expect {| |}]

let%expect_test "Loop overwrites require the later write on every relevant path"
    =
  assert_parameter_dependencies
    "for (n in 2:10) { v[n] = a; while (k > 0) break; v[n] = 0; if (v[n - 1] > \
     0) target += 1; }"
    [];
  assert_parameter_dependencies
    "for (n in 2:10) { if (k > 0) v[n] = a; v[n] = 0; if (v[n - 1] > 0) target \
     += 1; }"
    [];
  assert_parameter_dependencies
    "for (n in 2:10) { v[n] = a; if (k > 0) continue; v[n] = 0; if (v[n - 1] > \
     0) target += 1; }"
    [["a"]];
  assert_parameter_dependencies
    "for (n in 2:10) { v[n] = a; for (m in 1:k) v[n] = 0; if (v[n - 1] > 0) \
     target += 1; }"
    [["a"]];
  assert_parameter_dependencies
    "for (n in 2:10) { v[n] = a; if (k > 0) v[n] = 0; else v[n] = 1; if (v[n - \
     1] > 0) target += 1; }"
    [["a"]];
  [%expect {| |}]

let%expect_test "Earlier reads do not receive values from later iterations" =
  assert_parameter_dependencies
    "vector[10] c; vector[10] d; for (n in 1:9) { c[n] = v[n + 1]; d[n] = \
     v[n]; v[n] = a; if (c[n] > 0) target += 1; if (d[n] > 0) target += 1; if \
     (v[n] > 0) target += 1; }"
    [["a"]];
  assert_parameter_dependencies
    "vector[10] c; vector[10] d; for (n in 1:8) { c[10 - n] = v[10 - n - 1]; \
     d[10 - n] = v[10 - n]; v[10 - n] = a; if (c[10 - n] > 0) target += 1; if \
     (d[10 - n] > 0) target += 1; if (v[10 - n] > 0) target += 1; }"
    [["a"]];
  [%expect {| |}]

let%expect_test
    "Fixed read before the final possible writer does not depend on it" =
  assert_parameter_dependencies
    "for (n in 1:5) { if (v[5] > 0) target += 1; v[n] = a; }" [];
  [%expect {| |}]

let%expect_test
    "A write earlier in the body cannot kill a later writer's previous \
     iteration" =
  assert_parameter_dependencies
    "for (n in 2:10) { v[n] = 0; v[n] = a; if (v[n - 1] > 0) target += 1; }"
    [["a"]];
  [%expect {| |}]
