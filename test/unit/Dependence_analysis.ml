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
        ( reaching_defn_lookup x.reaching_defn_entry (VVar "j")
        , reaching_defn_lookup x.reaching_defn_exit (VVar "j") )) in
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
      ~f:(fun (_, x) -> (x.reaching_defn_entry, x.reaching_defn_exit)) in
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

(* ---- Subscript-aware reaching definitions (L4) ---- *)

(** For every node whose immediate dependencies are fewer than name-level
    reaching definitions would give, print the node, the definitions dropped
    because their subscripts cannot reach the node's reads, and the ones kept.
*)
let print_pruned_edges prog =
  let map = log_prob_build_dep_info_map (Test_utils.mir_of_string prog) in
  let name_level label =
    let stmt, info = LabelMap.find label map in
    let rhs =
      Analysis_and_optimization.Mir_utils.stmt_rhs_var_set stmt
      |> Set.Poly.map ~f:fst in
    Set.Poly.union info.parents
      (Set.Poly.union_map rhs
         ~f:(reaching_defn_lookup info.reaching_defn_entry)) in
  let pruned =
    LabelMap.fold map ~init:false ~f:(fun ~key:label ~data:_ pruned ->
        let plain = name_level label in
        let actual = node_immediate_dependencies map label in
        if Set.Poly.equal plain actual then pruned
        else (
          Fmt.pr "%d: dropped %a, kept %a@." label
            Fmt.(list ~sep:(any " ") int)
            (Set.Poly.to_list (Set.Poly.diff plain actual))
            Fmt.(list ~sep:(any " ") int)
            (Set.Poly.to_list actual);
          true)) in
  if not pruned then print_endline "no definition pruned"

let%expect_test "subscripts: distinct literal subscripts are independent" =
  print_pruned_edges
    {|
      data { real y; real s; }
      parameters { real a; real b; }
      model {
        vector[2] theta;
        theta[1] = a;
        theta[2] = b;
        y ~ normal(theta[1], s);
        y ~ normal(theta[2], s);
      }
    |};
  [%expect {|
    8: dropped 7, kept 1 5 6
    9: dropped 6, kept 1 5 7
    |}]

let%expect_test "subscripts: same-iteration definitions inside a loop" =
  print_pruned_edges
    {|
      data { int N; vector[N] x; vector[N] y; }
      parameters { real mu; real s; }
      model {
        vector[N] muj; vector[N] m;
        for (n in 1:N) {
          muj[n] = mu + x[n];
          m[n] = muj[n] * 2;
          y[n] ~ normal(m[n], s);
        }
      }
    |};
  [%expect {|
    no definition pruned
    |}]

let%expect_test "subscripts: whole-variable and confused accesses are kept" =
  print_pruned_edges
    {|
      data { int N; vector[N] x; array[N] int<lower=1, upper=N> idx; }
      parameters { real mu; }
      model {
        vector[N] v; real t;
        for (n in 1:N) {
          v[idx[n]] = mu + x[n];
          t = v[n];
        }
        target += t + sum(v);
      }
    |};
  [%expect {|
    no definition pruned
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
          ((filename string) (line_num 32) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 32) (col_num 17) (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 42) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 42) (col_num 17) (included_from ()))))
        k))
    |}]
