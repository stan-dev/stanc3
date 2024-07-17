open Core
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
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps = log_prob_dependency_graph example1_program in
  print_s [%sexp (deps : (label, label Set.Poly.t) Map.Poly.t)];
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 (3)) (5 (3 4)) (6 (3 4)) (7 (3 4)) (8 (3 4 10 12))
       (9 (3 4 8 10 12)) (10 (3 4 8 12)) (11 (3 4 8 10 12)) (12 (3 4 8 10))
       (13 (3 4 8 10 12)) (14 (3 4 8 10 12 13)) (15 (3 4 8 10 12 13))
       (16 (3 4 8 10 12 13 15)) (17 (3 4 8 10 12 13 15 16))
       (18 (3 4 8 10 12 13 15 16)) (19 (3 4 8 10 12 13 15 16))
       (20 (3 4 8 10 12 13 15 16)) (21 (3 4 8 10 12 13 15 16 18)))
    |}]

let%expect_test "Reaching defns example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    Map.Poly.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        ( reaching_defn_lookup x.reaching_defn_entry (VVar "j")
        , reaching_defn_lookup x.reaching_defn_exit (VVar "j") )) in
  print_s
    [%sexp (deps : (label, label Set.Poly.t * label Set.Poly.t) Map.Poly.t)];
  [%expect
    {|
      ((1 (() ())) (2 ((8) (8))) (3 (() ())) (4 (() ())) (5 (() ())) (6 (() ()))
       (7 ((8) (8))) (8 ((8) (8))) (9 ((8) (8))) (10 ((8) (8))) (11 ((8) (8)))
       (12 ((8) (8))) (13 ((8) (8))) (14 ((8) (8))) (15 ((8) (8))) (16 ((8) (8)))
       (17 ((8) (8))) (18 ((8) (8))) (19 ((8) (8))) (20 ((8) (8))) (21 ((8) (8))))
    |}]

let%expect_test "Reaching defns example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    Map.Poly.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) -> (x.reaching_defn_entry, x.reaching_defn_exit)) in
  print_s
    [%sexp
      (deps
        : ( label
          , reaching_defn Set.Poly.t * reaching_defn Set.Poly.t )
          Map.Poly.t)];
  [%expect
    {|
      ((1 (() ())) (2 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (3 (() (((VVar i) 3)))) (4 ((((VVar i) 3)) (((VVar i) 3))))
       (5 ((((VVar i) 3)) (((VVar i) 3)))) (6 ((((VVar i) 3)) (((VVar i) 3))))
       (7 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (8 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (9 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (10 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (11 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (12 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (13 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (14 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (15 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (16 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (17 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (18 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (19 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (20 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8))))
       (21 ((((VVar i) 3) ((VVar j) 8)) (((VVar i) 3) ((VVar j) 8)))))
    |}]

let%expect_test "Variable dependency example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    node_vars_dependencies
      (log_prob_build_dep_info_map example1_program)
      (Set.Poly.singleton (VVar "j"))
      17 in
  print_s [%sexp (deps : label Set.Poly.t)];
  [%expect {|
      (3 4 8 10 12 13 15 16)
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
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps = mir_uninitialized_variables uninitialized_var_example in
  print_s [%sexp (deps : (Location_span.t * string) Set.Poly.t)];
  [%expect
    {|
      ((((begin_loc
          ((filename string) (line_num 15) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 15) (col_num 18) (included_from ()))))
        wu)
       (((begin_loc
          ((filename string) (line_num 26) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 26) (col_num 17) (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 27) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 27) (col_num 17) (included_from ()))))
        z)
       (((begin_loc
          ((filename string) (line_num 29) (col_num 14) (included_from ())))
         (end_loc
          ((filename string) (line_num 29) (col_num 15) (included_from ()))))
        z)
       (((begin_loc
          ((filename string) (line_num 32) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 32) (col_num 17) (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 33) (col_num 14) (included_from ())))
         (end_loc
          ((filename string) (line_num 33) (col_num 15) (included_from ()))))
        z)
       (((begin_loc
          ((filename string) (line_num 42) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 42) (col_num 17) (included_from ()))))
        k))
    |}]
