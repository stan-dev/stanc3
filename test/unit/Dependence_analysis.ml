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
          Set.Poly.filter_map definitions ~f:(fun (write_location, writer_id) ->
              Option.map writer_id ~f:(fun id ->
                  ( VVar
                      write_location
                        .Analysis_and_optimization.Dependence.variable
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

let print_parameter_dependencies body =
  let mir =
    Test_utils.mir_of_string
      ("data { int<lower=1, upper=10> k; } parameters { real a; } model { \
        vector[10] v = rep_vector(0, 10); " ^ body ^ " }") in
  let prefix = "A control flow statement depends on parameter(s): " in
  let warnings =
    Analysis_and_optimization.Pedantic_analysis.warn_pedantic mir
    |> List.filter_map ~f:(fun (_, message) ->
        if String.starts_with ~prefix message then Some message else None) in
  match warnings with
  | [] -> print_endline "Control-flow warnings: none"
  | messages -> List.iter messages ~f:print_endline

let%expect_test "Dependencies: reading another literal element does not warn" =
  print_parameter_dependencies {|v[1] = a;
      if (v[2] > 0) target += 1;|};
  [%expect {|
    Control-flow warnings: none
  |}]

let%expect_test "Dependencies: reading the written element warns" =
  print_parameter_dependencies {|v[1] = a;
      if (v[1] > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test
    "Dependencies: overwriting a stable element removes its parameter \
     dependency" =
  print_parameter_dependencies
    {|v[k] = a;
      v[k] = 0;
      if (v[k] > 0) target += 1;|};
  [%expect {|
    Control-flow warnings: none
  |}]

let%expect_test "Dependencies: changing an index preserves the earlier element"
    =
  print_parameter_dependencies
    {|int i = 1;
      v[i] = a;
      i = 2;
      v[i] = 0;
      if (v[1] > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test
    "Dependencies: changing an index array preserves the earlier element" =
  print_parameter_dependencies
    {|array[2] int idx = {1, 2};
      v[idx[1]] = a;
      idx[1] = 2;
      v[idx[1]] = 0;
      if (v[1] > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test
    "Conservative dependencies: loop bounds do not yet prove disjointness" =
  print_parameter_dependencies
    {|for (n in 1:5) {
        v[n + 5] = a;
        if (v[n] > 0) target += 1;
      }|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test "Dependencies: an earlier iteration can supply a read" =
  print_parameter_dependencies
    {|for (n in 2:5) {
        if (v[n - 1] > 0) target += 1;
        v[n] = a;
      }|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test
    "Conservative dependencies: same-iteration overwrites are not yet proved" =
  print_parameter_dependencies
    {|for (n in 1:5) {
        v[n] = a;
        v[n] = 0;
        if (v[n] > 0) target += 1;
      }|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test "Dependencies: a conditional overwrite might not execute" =
  print_parameter_dependencies
    {|v[1] = a;
      if (k > 1) v[1] = 0;
      if (v[1] > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test "Dependencies: nested slices still read the original vector" =
  print_parameter_dependencies
    {|v[2] = a;
      if (v[2:3][1] > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test
    "Conservative dependencies: slice overwrites do not yet remove definitions"
    =
  print_parameter_dependencies
    {|v[1] = a;
      v[1:2] = rep_vector(0, 2);
      if (v[1] > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test "Dependencies: writing another tuple field does not warn" =
  print_parameter_dependencies
    {|tuple(real, real) t;
      t.1 = a;
      t.2 = 0;
      if (t.2 > 0) target += 1;|};
  [%expect {|
    Control-flow warnings: none
  |}]

let%expect_test
    "Dependencies: another tuple field does not overwrite the parameter" =
  print_parameter_dependencies
    {|tuple(real, real) t;
      t.1 = a;
      t.2 = 0;
      if (t.1 > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test "Dependencies: disjoint ranges do not warn" =
  print_parameter_dependencies
    {|v[1:2] = rep_vector(a, 2);
      if (sum(v[3:4]) > 0) target += 1;|};
  [%expect {|
    Control-flow warnings: none
  |}]

let%expect_test "Dependencies: disjoint index arrays do not warn" =
  print_parameter_dependencies
    {|v[{1, 3}] = rep_vector(a, 2);
      if (sum(v[{2, 4}]) > 0) target += 1;|};
  [%expect {|
    Control-flow warnings: none
  |}]

let%expect_test "Dependencies: ranges sharing an endpoint warn" =
  print_parameter_dependencies
    {|v[1:2] = rep_vector(a, 2);
      if (sum(v[2:3]) > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test
    "Dependencies: a symbolic array element can select the read position" =
  print_parameter_dependencies
    {|v[{1, k}] = rep_vector(a, 2);
      if (v[2] > 0) target += 1;|};
  [%expect {|
    A control flow statement depends on parameter(s): a.
  |}]

let%expect_test
    "Random subscripts: the actual indexed read retains all three writers" =
  let mir =
    Test_utils.mir_of_string
      {|functions {
        real pick_rng(vector theta) {
          vector[num_elements(theta) + 1] v = rep_vector(0, num_elements(theta) + 1);
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
          Fmt.pr "%a@.Possible writers:@." pp_statement pattern;
          let read =
            List.find_opt info.accesses.reads ~f:(fun location ->
                String.equal
                  location.Analysis_and_optimization.Dependence.variable "v")
            |> Option.get in
          reaching_defn_lookup info.reaching_defn_entry read
          |> Set.Poly.iter ~f:(fun writer_id ->
              let writer, _ = LabelMap.find writer_id statements in
              Fmt.pr "  %a@." pp_statement writer)
      | _ -> ());
  [%expect
    {|
    return v[(categorical_rng(theta) + 1)];
    Possible writers:
      v = rep_vector(0, (num_elements(theta) + 1));
      v[categorical_rng(theta)] = 1;
      v[categorical_rng(theta)] = 2;
    |}]
