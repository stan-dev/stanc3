open Std
open Std.Sexp_conv
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_utils
open Middle
open Analysis_and_optimization.Dataflow_types

(* These tests check {!Analysis_and_optimization.Dependence_analysis}. The
   module finds the earlier statements that each statement depends on. Pedantic
   mode and the factor graph use these statement dependencies.

   Each test builds a map from labels to statements with
   [log_prob_build_dep_info_map]. A label is the integer that identifies a
   statement. The map covers the log density, which contains the statements of
   the parameters, transformed parameters and model blocks. The // comments in
   the first program give the labels. Label 1 is the root statement of the map,
   which holds all the other statements.

   A statement has a data dependency on an earlier write when the write can
   supply a value that the statement reads. Such a write flows to the read. A
   statement has a control dependency on an enclosing if or loop, and on a jump
   that can skip the statement. A jump is a [break], [continue] or [return].
   These ifs, loops and jumps are the controls of the statement. An immediate
   dependency is a direct data or control dependency. A transitive dependency is
   a dependency of a dependency.

   A definition is a write, recorded as the variable name and the label of the
   statement that writes the variable. A definition reaches a read when the
   program can run the write before the read, and no write between them replaces
   the same element. These definitions are the reaching definitions of the read.
   Each statement records the reaching definitions at entry, before the
   statement runs, and at exit, after the statement runs. A later write kills an
   earlier write when the later write always replaces the element of the earlier
   write. The later write is the killer. A killed write no longer reaches later
   reads. A query then drops each reaching write that selects a different
   element than the read, or that runs after the read
   ({!Analysis_and_optimization.Dependence}).

   A definition from outside the analyzed statement, for example data and
   parameters, has no label and prints as [()] in a reaching definition.
   Dependency results contain only labels. Thus the analysis uses the root label
   as a marker for these definitions. Every map has the root, so the marker is
   always a valid label.

   Most later tests print [label: statement // labels]. The labels after // are
   the immediate dependencies, which are the writes that the statement reads and
   the statements that control the statement. *)

let example1_program =
  Test_utils.mir_of_string
    {|
        model
        {                                // 2
          int i                          // 3
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

let%expect_test "Reaching definition labels for the loop variable" =
  let deps =
    LabelMap.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        ( reaching_defn_lookup x.reaching_definitions.entry "j"
        , reaching_defn_lookup x.reaching_definitions.exit "j" )) in
  print_s
    [%sexp
      (deps : (label option Set.Poly.t * label option Set.Poly.t) LabelMap.t)];
  [%expect
    {|
    ((1 (() ())) (2 (((9)) ((9)))) (3 (() ())) (4 (() ())) (5 (() ()))
     (6 (() ())) (7 (() ())) (8 (((9)) ((9)))) (9 (((9)) ((9))))
     (10 (((9)) ((9)))) (11 (((9)) ((9)))) (12 (((9)) ((9)))) (13 (((9)) ((9))))
     (14 (((9)) ((9)))) (15 (((9)) ((9)))) (16 (((9)) ((9)))) (17 (((9)) ((9))))
     (18 (((9)) ((9)))) (19 (((9)) ((9)))) (20 (((9)) ((9)))) (21 (((9)) ((9))))
     (22 (((9)) ((9)))))
    |}]

let%expect_test "Reaching definition records at statement entry and exit" =
  let deps =
    LabelMap.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        (x.reaching_definitions.entry, x.reaching_definitions.exit)) in
  print_s
    [%sexp
      (deps : (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) LabelMap.t)];
  [%expect
    {|
    ((1 (() ())) (2 (((i (4)) (j (9))) ((i (4)) (j (9))))) (3 (() ((i (3)))))
     (4 (((i (3))) ((i (4))))) (5 (((i (4))) ((i (4)))))
     (6 (((i (4))) ((i (4))))) (7 (((i (4))) ((i (4)))))
     (8 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (9 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (10 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (11 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (12 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (13 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (14 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (15 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (16 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (17 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (18 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (19 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (20 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (21 (((i (4)) (j (9))) ((i (4)) (j (9)))))
     (22 (((i (4)) (j (9))) ((i (4)) (j (9))))))
    |}]

let%expect_test "Variable dependency example" =
  let deps =
    node_vars_dependencies
      (log_prob_build_dep_info_map example1_program)
      (Set.Poly.singleton "j") 17 in
  print_s [%sexp (deps : label Set.Poly.t)];
  [%expect {|
      (4 5 9 11 13 14 16)
    |}]

let%expect_test "External definitions are distinct from statement label 1" =
  let definitions =
    Set.Poly.of_list [("x", None); ("x", Some 1); ("y", Some 2)] in
  print_s
    [%sexp (reaching_defn_lookup definitions "x" : label option Set.Poly.t)];
  [%expect {| (() (1)) |}]

let%expect_test "External inputs remain dependencies after map construction" =
  let map =
    log_prob_build_dep_info_map
      (Test_utils.mir_of_string
         {|
           data { int N; }
           parameters { real mu; }
           model { if (N > 0) target += mu; }
         |})
  in
  LabelMap.iter map ~f:(fun ~key:statement_id ~data:(pattern, info) ->
      match pattern with
      | Stmt.Pattern.IfElse _ ->
          print_s
            [%sexp
              (reaching_defn_lookup info.reaching_definitions.entry "N"
                : label option Set.Poly.t)];
          Fmt.pr "dependencies: %a@." Test_utils.pp_labels
            (node_immediate_dependencies map statement_id)
      | _ -> ());
  [%expect {|
    (())
    dependencies: 1
    |}]

(** Prints each statement and the labels of the immediate dependencies of the
    statement. *)
let pp_immediate_dependencies ppf (map : dep_info_map) =
  LabelMap.iter map ~f:(fun ~key:statement_id ~data:(pattern, _) ->
      Fmt.pf ppf "%d: %a  // %a@." statement_id Test_utils.pp_stmt pattern
        Test_utils.pp_labels
        (node_immediate_dependencies map statement_id))

let print_immediate_dependencies prog =
  Fmt.pr "%a" pp_immediate_dependencies
    (log_prob_build_dep_info_map (Test_utils.mir_of_string prog))

let%expect_test "Transitive dependencies of an if and of its condition" =
  let map =
    log_prob_build_dep_info_map
      (Test_utils.mir_of_string
         {|
      parameters { real mu; real sigma; }
      model {
        real a = mu + 1;
        real b = a * 2;
        real c = sigma;
        if (b > 0)
          target += normal_lpdf(c | 0, 1);
      }
    |})
  in
  let if_id =
    LabelMap.fold map ~init:0
      ~f:(fun ~key:statement_id ~data:(pattern, _) found_id ->
        match pattern with
        | Stmt.Pattern.IfElse _ -> statement_id
        | _ -> found_id) in
  Fmt.pr "%a" pp_immediate_dependencies map;
  Fmt.pr "node_dependencies %d: %a@." if_id Test_utils.pp_labels
    (node_dependencies map if_id);
  Fmt.pr "node_vars_dependencies {b} %d: %a@." if_id Test_utils.pp_labels
    (node_vars_dependencies map (Set.Poly.singleton "b") if_id);
  Fmt.pr "node_vars_dependencies ~blockers:{b} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (node_vars_dependencies map ~blockers:(Set.Poly.singleton "b")
       (Set.Poly.singleton "b") if_id);
  Fmt.pr "node_vars_dependencies ~blockers:{a} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (node_vars_dependencies map ~blockers:(Set.Poly.singleton "a")
       (Set.Poly.singleton "b") if_id);
  Fmt.pr "node_vars_dependencies ~blockers:{mu} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (node_vars_dependencies map ~blockers:(Set.Poly.singleton "mu")
       (Set.Poly.singleton "b") if_id);
  (* The condition does not read c. Thus a query for c keeps each write of c
     that reaches the condition. *)
  Fmt.pr "node_vars_dependencies {c} %d: %a@." if_id Test_utils.pp_labels
    (node_vars_dependencies map (Set.Poly.singleton "c") if_id);
  [%expect
    {|
    1: 2 3 4  //
    2: real mu;  //
    3: real sigma;  //
    4: { 5 6 7 8 9 10 11 }  //
    5: real a;  //
    6: a = (mu + promote(1, real, data));  // 2
    7: real b;  //
    8: b = (a * promote(2, real, data));  // 6
    9: real c;  //
    10: c = sigma;  // 3
    11: if((b > 0)) 12  // 8
    12: target += normal_lpdf(c, promote(0, real, data), promote(1, real, data));  // 10 11
    node_dependencies 11: 2 6 8 11
    node_vars_dependencies {b} 11: 2 6 8
    node_vars_dependencies ~blockers:{b} {b} 11:
    node_vars_dependencies ~blockers:{a} {b} 11: 8
    node_vars_dependencies ~blockers:{mu} {b} 11: 6 8
    node_vars_dependencies {c} 11: 3 10
    |}]

let%expect_test
    "Reaching definitions: an _lp call in an assignment defines target" =
  let map =
    log_prob_build_dep_info_map
      (Test_utils.mir_of_string
         {|
           functions { real foo_lp(real y) { target += y; return y; } }
           parameters { real mu; }
           model {
             real x;
             x = foo_lp(mu);
             if (target() > 0) target += 1;
           }
         |})
  in
  LabelMap.iter map ~f:(fun ~key:statement_id ~data:(stmt, _) ->
      match stmt with
      | Stmt.Pattern.IfElse _ ->
          Fmt.pr "%d: %a@." statement_id Test_utils.pp_labels
            (node_immediate_dependencies map statement_id)
      | _ -> ());
  [%expect {| 6: 5 |}]

(* The Graph tests use programs from test/unit/Dependence.ml. A write is not a
   dependency of a read if the write selects a different element, or if the
   write runs after the read. *)

let%expect_test "Graph: definitions that execute after the read never flow" =
  print_immediate_dependencies
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d;
        for (n in 1:(N - 1)) {
          c[n] = b[n + 1];
          d[n] = b[n];
          b[n] = a;
        }
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 7 8 9 10 }  //
    4: FnValidateSize__("b", "N", N);  // 1
    5: vector[N] b;  // 1
    6: FnValidateSize__("c", "N", N);  // 1
    7: vector[N] c;  // 1
    8: FnValidateSize__("d", "N", N);  // 1
    9: vector[N] d;  // 1
    10: for(n in 1:(N - 1)) 11  // 1
    11: { 12 13 14 }  // 10
    12: c[n] = b[(n + 1)];  // 5 10
    13: d[n] = b[n];  // 5 10
    14: b[n] = a;  // 2 10
    |}]

let%expect_test "Graph: a definition in a later outer iteration never flows" =
  print_immediate_dependencies
    {|
      data { int N; int M; }
      parameters { real a; }
      model {
        vector[N] b; matrix[N, M] c; matrix[N, M] d;
        for (n in 2:(N - 1)) {
          for (m in 1:M) {
            c[n, m] = b[n + 1];
            d[n, m] = b[n - 1];
            b[n] = a;
          }
        }
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 7 8 9 10 11 12 }  //
    4: FnValidateSize__("b", "N", N);  // 1
    5: vector[N] b;  // 1
    6: FnValidateSize__("c", "N", N);  // 1
    7: FnValidateSize__("c", "M", M);  // 1
    8: matrix[N, M] c;  // 1
    9: FnValidateSize__("d", "N", N);  // 1
    10: FnValidateSize__("d", "M", M);  // 1
    11: matrix[N, M] d;  // 1
    12: for(n in 2:(N - 1)) 13  // 1
    13: { 14 }  // 12
    14: for(m in 1:M) 15  // 1 12
    15: { 16 17 18 }  // 14
    16: c[n, m] = b[(n + 1)];  // 5 14
    17: d[n, m] = b[(n - 1)];  // 5 14 18
    18: b[n] = a;  // 2 14
    |}]

let%expect_test "Graph: distinct literal subscripts are independent" =
  print_immediate_dependencies
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
  [%expect
    {|
    1: 2 3 4  //
    2: real a;  //
    3: real b;  //
    4: { 5 6 7 8 9 }  //
    5: vector[2] theta;  //
    6: theta[1] = a;  // 2
    7: theta[2] = b;  // 3
    8: target += normal_lupdf(y, theta[1], s);  // 1 5 6
    9: target += normal_lupdf(y, theta[2], s);  // 1 5 7
    |}]

(* An increment of target does not use the value of an earlier increment. Thus
   the second increment does not depend on the first. *)
let%expect_test "Graph: increments of target do not depend on each other" =
  print_immediate_dependencies
    {|
      parameters { real a; real b; }
      model {
        target += a;
        target += b;
      }
    |};
  [%expect
    {|
    1: 2 3 4  //
    2: real a;  //
    3: real b;  //
    4: { 5 6 }  //
    5: target += a;  // 2
    6: target += b;  // 3
    |}]

let%expect_test "Variables read at a set of labels" =
  let map =
    log_prob_build_dep_info_map
      (Test_utils.mir_of_string
         {|
      data { int N; int k; vector[N] x; }
      parameters { real mu; }
      model {
        vector[N] v;
        v[k] = x[k] + mu;
        for (n in 1:N) v[n] = x[n];
      }
    |})
  in
  LabelMap.iter map ~f:(fun ~key:statement_id ~data:(pattern, _) ->
      Fmt.pr "%d: %a  // %a@." statement_id Test_utils.pp_stmt pattern
        Fmt.(list ~sep:(any " ") string)
        (Set.Poly.to_list
           (read_variables_at map (Set.Poly.singleton statement_id))));
  [%expect
    {|
    1: 2 3  //
    2: real mu;  //
    3: { 4 5 6 7 }  //
    4: FnValidateSize__("v", "N", N);  // N
    5: vector[N] v;  // N
    6: v[k] = (x[k] + mu);  // k mu x
    7: for(n in 1:N) 8  // N
    8: { 9 }  //
    9: v[n] = x[n];  // x
    |}]

let%expect_test "Running example of the Dependence_analysis interface" =
  let program =
    {|
      data { int N; vector[N] x; }
      parameters { real a; }
      model {
        matrix[N, 2] theta;
        for (n in 1:N) {
          theta[n, 1] = a;
          theta[n, 2] = x[n];
          if (theta[n, 2] > 0) target += 1;
        }
      }
    |}
  in
  print_immediate_dependencies program;
  let map = log_prob_build_dep_info_map (Test_utils.mir_of_string program) in
  let condition_info = snd (LabelMap.find 10 map) in
  let dependency_labels = node_dependencies map 10 in
  Fmt.pr "theta definitions reaching 10: %a@.dependencies of 10: %a@.read: %a@."
    Fmt.(list ~sep:(any " ") (option ~none:(any "None") int))
    (Set.Poly.to_list
       (reaching_defn_lookup condition_info.reaching_definitions.entry "theta"))
    Test_utils.pp_labels dependency_labels
    Fmt.(list ~sep:(any " ") string)
    (Set.Poly.to_list (read_variables_at map dependency_labels));
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 }  //
    4: FnValidateSize__("theta", "N", N);  // 1
    5: matrix[N, 2] theta;  // 1
    6: for(n in 1:N) 7  // 1
    7: { 8 9 10 }  // 6
    8: theta[n, 1] = a;  // 2 6
    9: theta[n, 2] = x[n];  // 1 6
    10: if((theta[n, 2] > 0)) 11  // 5 6 9
    11: target += 1;  // 10
    theta definitions reaching 10: 5 8 9
    dependencies of 10: 1 5 6 9 10
    read: N theta x
    |}]

(* The Kills tests check that a killed write is not a dependency of a read after
   the later write. *)

let%expect_test "Kills: the same element in every iteration" =
  print_immediate_dependencies
    {|
      data { int N; int k; }
      parameters { real a; row_vector[N] r; }
      model {
        vector[N] theta; matrix[N, N] m; tuple(real, real) t; int j = k;
        theta[1] = a;
        theta[1] = 1;
        if (theta[1] > 0) target += 1;   // theta[1] = 1 removes theta[1] = a
        theta[k] = a;
        theta[k] = 1;
        if (theta[k] > 0) target += 1;   // k does not change, so theta[k] = a is removed
        theta[j] = a;
        j = 2;
        theta[j] = 1;
        if (theta[1] > 0) target += 1;   // j changes, so theta[j] = a remains
        t.1 = a;
        t.1 = 1;
        if (t.1 > 0) target += 1;        // the same field, so t.1 = a is removed
        m[1, 2] = a;
        m[1] = r;
        if (m[1, 2] > 0) target += 1;    // m[1] contains m[1, 2], so m[1, 2] = a is removed
        m[2] = r;
        m[2, 2] = 1;
        if (m[2, 3] > 0) target += 1;    // m[2, 2] is part of the row, so m[2] = r remains
      }
    |};
  [%expect
    {|
    1: 2 3 4  //
    2: real a;  //
    3: row_vector[N] r;  // 1
    4: { 5 6 7 8 9 10 11 12 13 14 15 17 18 19 21 22 23 24 26 27 28 30 31 32 34 35 36 }  //
    5: FnValidateSize__("theta", "N", N);  // 1
    6: vector[N] theta;  // 1
    7: FnValidateSize__("m", "N", N);  // 1
    8: FnValidateSize__("m", "N", N);  // 1
    9: matrix[N, N] m;  // 1
    10: tuple_ad(var, var)tuple(real, real) t;  //
    11: int j;  //
    12: j = k;  // 1
    13: theta[1] = a;  // 2
    14: theta[1] = promote(1, real, var);  //
    15: if((theta[1] > 0)) 16  // 6 14
    16: target += 1;  // 15
    17: theta[k] = a;  // 1 2
    18: theta[k] = promote(1, real, var);  // 1
    19: if((theta[k] > 0)) 20  // 1 6 14 18
    20: target += 1;  // 19
    21: theta[j] = a;  // 2 12
    22: j = 2;  //
    23: theta[j] = promote(1, real, var);  // 22
    24: if((theta[1] > 0)) 25  // 6 14 18 21 23
    25: target += 1;  // 24
    26: t.1 = a;  // 2
    27: t.1 = promote(1, real, var);  //
    28: if((t.1 > 0)) 29  // 10 27
    29: target += 1;  // 28
    30: m[1, 2] = a;  // 2
    31: m[1] = r;  // 3
    32: if((m[1, 2] > 0)) 33  // 9 31
    33: target += 1;  // 32
    34: m[2] = r;  // 3
    35: m[2, 2] = promote(1, real, var);  //
    36: if((m[2, 3] > 0)) 37  // 9 31 34
    37: target += 1;  // 36
    |}]

let%expect_test "Kills: control flow between the writes" =
  print_immediate_dependencies
    {|
      data { int N; int c; }
      parameters { real a; }
      model {
        vector[N] theta;
        theta[1] = a;
        if (c > 0) target += 1;
        theta[1] = 1;
        if (theta[1] > 0) target += 1;   // replaced after the if, so removed
        theta[2] = a;
        if (c > 0) theta[2] = 1; else theta[2] = 2;
        if (theta[2] > 0) target += 1;   // replaced in both branches, so removed
        theta[3] = a;
        if (c > 0) theta[3] = 1;
        if (theta[3] > 0) target += 1;   // replaced in one branch only, so theta[3] = a remains
        theta[1:2] = [a, a]';
        theta[1:2] = [1, 1]';
        if (theta[1] > 0) target += 1;   // a slice never removes a write, so theta[1:2] = [a, a]' remains
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 7 9 10 12 13 16 18 19 21 23 24 25 }  //
    4: FnValidateSize__("theta", "N", N);  // 1
    5: vector[N] theta;  // 1
    6: theta[1] = a;  // 2
    7: if((c > 0)) 8  // 1
    8: target += 1;  // 7
    9: theta[1] = promote(1, real, var);  //
    10: if((theta[1] > 0)) 11  // 5 9
    11: target += 1;  // 10
    12: theta[2] = a;  // 2
    13: if((c > 0)) 14 else 15  // 1
    14: theta[2] = promote(1, real, var);  // 13
    15: theta[2] = promote(2, real, var);  // 13
    16: if((theta[2] > 0)) 17  // 5 14 15
    17: target += 1;  // 16
    18: theta[3] = a;  // 2
    19: if((c > 0)) 20  // 1
    20: theta[3] = promote(1, real, var);  // 19
    21: if((theta[3] > 0)) 22  // 5 18 20
    22: target += 1;  // 21
    23: theta[1:2] = (FnMakeRowVec__(a, a) ');  // 2
    24: theta[1:2] = (FnMakeRowVec__(promote(1, real, data), promote(1, real, data)) ');  //
    25: if((theta[1] > 0)) 26  // 5 9 23 24
    26: target += 1;  // 25
    |}]

let%expect_test "Kills: jumps and branches between the two writes" =
  print_immediate_dependencies
    {|
      data { int N; int c; }
      parameters { real a; }
      model {
        vector[N] theta;
        for (n in 2:N) {
          theta[n] = a;
          while (c > 0) break;
          theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // the break leaves only the while, so removed
        }
        for (n in 2:N) {
          if (c > 0) theta[n] = a;
          theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // replaced after the if, so removed
        }
        for (n in 2:N) {
          theta[n] = a;
          if (c > 0) theta[n] = 1; else theta[n] = 2;
          if (theta[n - 1] > 0) target += 1;  // a killer inside an if does not count, so theta[n] = a remains
        }
        for (n in 2:N) {
          theta[n] = a;
          if (c > 0) theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // only one branch replaces theta[n], so theta[n] = a remains
        }
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 14 21 29 }  //
    4: FnValidateSize__("theta", "N", N);  // 1
    5: vector[N] theta;  // 1
    6: for(n in 2:N) 7  // 1
    7: { 8 9 11 12 }  // 6
    8: theta[n] = a;  // 2 6
    9: while((c > 0)) 10  // 1 6 10
    10: break;  // 9
    11: theta[n] = promote(1, real, var);  // 6
    12: if((theta[(n - 1)] > 0)) 13  // 5 6 11
    13: target += 1;  // 12
    14: for(n in 2:N) 15  // 1
    15: { 16 18 19 }  // 14
    16: if((c > 0)) 17  // 1 14
    17: theta[n] = a;  // 2 16
    18: theta[n] = promote(1, real, var);  // 14
    19: if((theta[(n - 1)] > 0)) 20  // 5 11 14 18
    20: target += 1;  // 19
    21: for(n in 2:N) 22  // 1
    22: { 23 24 27 }  // 21
    23: theta[n] = a;  // 2 21
    24: if((c > 0)) 25 else 26  // 1 21
    25: theta[n] = promote(1, real, var);  // 24
    26: theta[n] = promote(2, real, var);  // 24
    27: if((theta[(n - 1)] > 0)) 28  // 5 11 18 21 23 25 26
    28: target += 1;  // 27
    29: for(n in 2:N) 30  // 1
    30: { 31 32 34 }  // 29
    31: theta[n] = a;  // 2 29
    32: if((c > 0)) 33  // 1 29
    33: theta[n] = promote(1, real, var);  // 32
    34: if((theta[(n - 1)] > 0)) 35  // 5 11 18 23 25 26 29 31 33
    35: target += 1;  // 34
    |}]

let%expect_test "Kills: loop-variable indices within one iteration" =
  print_immediate_dependencies
    {|
      data { int N; int c; }
      parameters { real a; }
      model {
        vector[N] theta; vector[N] y;
        for (n in 2:N) {
          theta[n] = a;
          theta[n] = 1;
          if (theta[n] > 0) target += 1;    // replaced in the same iteration, so removed
        }
        for (n in 2:N) {
          theta[n] = a;
          if (c > 0) y[n] = 1; else y[n] = 2;
          theta[n] = 1;
          if (theta[n] > 0) target += 1;    // replaced after the if, so removed
        }
        for (n in 2:N) {
          theta[n] = 1;
          theta[n] = a;
          y[n] = theta[n - 1];              // theta[n] = a of the previous iteration remains
        }
        for (n in 2:N) {
          theta[n] = a;
          if (c > 0) continue;
          theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // continue can skip theta[n] = 1, so theta[n] = a remains
        }
        for (n in 2:N) {
          theta[n] = a;
          for (m in 1:N) theta[n] = 1;
          if (theta[n - 1] > 0) target += 1;  // the inner loop can run zero times, so theta[n] = a remains
        }
      }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real a;  //
    3: { 4 5 6 7 8 14 23 28 36 }  //
    4: FnValidateSize__("theta", "N", N);  // 1
    5: vector[N] theta;  // 1
    6: FnValidateSize__("y", "N", N);  // 1
    7: vector[N] y;  // 1
    8: for(n in 2:N) 9  // 1
    9: { 10 11 12 }  // 8
    10: theta[n] = a;  // 2 8
    11: theta[n] = promote(1, real, var);  // 8
    12: if((theta[n] > 0)) 13  // 5 8 11
    13: target += 1;  // 12
    14: for(n in 2:N) 15  // 1
    15: { 16 17 20 21 }  // 14
    16: theta[n] = a;  // 2 14
    17: if((c > 0)) 18 else 19  // 1 14
    18: y[n] = promote(1, real, var);  // 17
    19: y[n] = promote(2, real, var);  // 17
    20: theta[n] = promote(1, real, var);  // 14
    21: if((theta[n] > 0)) 22  // 5 11 14 20
    22: target += 1;  // 21
    23: for(n in 2:N) 24  // 1
    24: { 25 26 27 }  // 23
    25: theta[n] = promote(1, real, var);  // 23
    26: theta[n] = a;  // 2 23
    27: y[n] = theta[(n - 1)];  // 5 11 20 23 26
    28: for(n in 2:N) 29  // 1
    29: { 30 31 33 34 }  // 28
    30: theta[n] = a;  // 2 28
    31: if((c > 0)) 32  // 1 28
    32: continue;  // 31
    33: theta[n] = promote(1, real, var);  // 28 32
    34: if((theta[(n - 1)] > 0)) 35  // 5 11 20 26 28 30 32 33
    35: target += 1;  // 32 34
    36: for(n in 2:N) 37  // 1
    37: { 38 39 42 }  // 36
    38: theta[n] = a;  // 2 36
    39: for(m in 1:N) 40  // 1 36
    40: { 41 }  // 39
    41: theta[n] = promote(1, real, var);  // 39
    42: if((theta[(n - 1)] > 0)) 43  // 5 11 20 26 30 33 36 38 41
    43: target += 1;  // 42
    |}]

(* Each statement prints the controls, then the two views that queries use. The
   controls list the enclosing ifs and loops, outermost first, then the jumps.
   [control_parents] keeps the nearest enclosing if or loop and the jumps.
   [loops_around] keeps the loops, and a loop statement adds itself. *)
let%expect_test "Controls and the two views derived from them" =
  let map =
    log_prob_build_dep_info_map
      (Test_utils.mir_of_string
         {|
      data { int N; int c; }
      parameters { real a; }
      model {
        vector[N] v;
        for (n in 1:N) {
          if (c > 0) continue;
          while (v[n] < 0) {
            if (c > 1) v[n] = a;
          }
        }
      }
    |})
  in
  let pp_enclosing ppf = function
    | If {id; branch= Then} -> Fmt.pf ppf "if %d then" id
    | If {id; branch= Else} -> Fmt.pf ppf "if %d else" id
    | Loop (For {id; var}) -> Fmt.pf ppf "loop %d (%s)" id var
    | Loop (While id) -> Fmt.pf ppf "loop %d (while)" id in
  let pp_controls ppf {enclosing; jumps} =
    Fmt.(list ~sep:(any "; ") string)
      ppf
      (List.map enclosing ~f:(Fmt.str "%a" pp_enclosing)
      @ List.map (Set.Poly.to_list jumps) ~f:(Fmt.str "jump %d")) in
  let pp_loop ppf (loop : loop) = Fmt.int ppf (loop_id loop) in
  LabelMap.iter map ~f:(fun ~key:statement_id ~data:(pattern, info) ->
      Fmt.pr "%d: %a@.   controls [%a]  dependencies %a  loops [%a]@."
        statement_id Test_utils.pp_stmt pattern pp_controls info.controls
        Test_utils.pp_labels
        (control_parents info.controls)
        Fmt.(list ~sep:(any "; ") pp_loop)
        (loops_around ~statement_id pattern info.controls));
  [%expect
    {|
    1: 2 3
       controls []  dependencies   loops []
    2: real a;
       controls []  dependencies   loops []
    3: { 4 5 6 }
       controls []  dependencies   loops []
    4: FnValidateSize__("v", "N", N);
       controls []  dependencies   loops []
    5: vector[N] v;
       controls []  dependencies   loops []
    6: for(n in 1:N) 7
       controls []  dependencies   loops [6]
    7: { 8 10 }
       controls [loop 6 (n)]  dependencies 6  loops [6]
    8: if((c > 0)) 9
       controls [loop 6 (n)]  dependencies 6  loops [6]
    9: continue;
       controls [loop 6 (n); if 8 then]  dependencies 8  loops [6]
    10: while((v[n] < 0)) 11
       controls [loop 6 (n); jump 9]  dependencies 6 9  loops [6; 10]
    11: { 12 }
       controls [loop 6 (n); loop 10 (while); jump 9]  dependencies 9 10  loops [6; 10]
    12: if((c > 1)) 13
       controls [loop 6 (n); loop 10 (while); jump 9]  dependencies 9 10  loops [6; 10]
    13: v[n] = a;
       controls [loop 6 (n); loop 10 (while); if 12 then; jump 9]  dependencies 9 12  loops [6; 10]
    |}]
