open Std
open Std.Sexp_conv
open Analysis_and_optimization.Dependence_analysis
open Middle
open Analysis_and_optimization.Dataflow_types

(* These tests check statement dependencies used by pedantic mode and the factor
   graph. Expected results show whether element and execution-order checks
   retain the correct candidate definitions.

   Comments in the first program identify statement labels. Label 1 identifies
   the analyzed log-density statement, which contains the nested statements.
   Later tests print direct results as [label: statement // dependency
   labels]. *)

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

let%expect_test "Reaching definition labels for the loop variable" =
  let deps =
    LabelMap.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        ( reaching_defn_lookup x.reaching_defn_entry "j"
        , reaching_defn_lookup x.reaching_defn_exit "j" )) in
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
      ~f:(fun (_, x) -> (x.reaching_defn_entry, x.reaching_defn_exit)) in
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
              (reaching_defn_lookup info.reaching_defn_entry "N"
                : label option Set.Poly.t)];
          Fmt.pr "dependencies: %a@." Test_utils.pp_labels
            (node_immediate_dependencies map statement_id)
      | _ -> ());
  [%expect {|
    (())
    dependencies: 1
    |}]

(** Prints each statement beside the labels returned by its direct-dependency
    query. For a condition at label [4] supplied only by assignment [3], the
    dependency column contains [3]. *)
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
  (* The condition does not read c. A query explicitly requesting c therefore
     has no sink access to compare and retains every reaching definition of
     c. *)
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

(* Check element and ordering results in the statement graph.

   The same programs in test/unit/Dependence.ml print individual access-pair
   verdicts. Here the dependency-label column checks the resulting graph edges.
   Matching variable names supply candidates; incompatible elements or execution
   order must remove the corresponding assignment labels. *)

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

(* Check candidate fallback separately from access-kind filtering. A source
   without a write or a sink without a read is retained by name. Recorded
   accesses must form an eligible pair, so separate target increments cannot
   introduce dependencies on each other. *)
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
