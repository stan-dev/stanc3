open Std
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_utils
open Middle
open Analysis_and_optimization.Dataflow_types

(* These tests check {!Analysis_and_optimization.Dependence_analysis}, which
   finds the earlier statements that each statement depends on.

   Each test builds the statement records of the log density with
   [build_log_prob_dependence_info] and prints one line per statement: [label:
   statement // data 5 9 control 6]. The labels after [data] are the writers
   whose value the statement can read, and the labels after [control] are the
   if, loop or jump that decides whether the statement runs. A query prints as
   [name args: labels]. The // comments in the first program give the labels,
   and label 1 is the root statement that holds all the others. *)

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

(** Prints each statement and the immediate dependences of the statement. *)
let pp_immediate_dependencies ppf (statements : stmt_info LabelMap.t) =
  LabelMap.iter statements ~f:(fun ~key:statement_id ~data:statement ->
      Fmt.pf ppf "%d: %a  // %a@." statement_id Test_utils.pp_stmt
        statement.pattern pp_dependences statement)

let print_immediate_dependencies prog =
  Fmt.pr "%a" pp_immediate_dependencies
    (build_log_prob_dependence_info (Test_utils.mir_of_string prog))

let%expect_test "Dependency graph example" =
  let statements = build_log_prob_dependence_info example1_program in
  (* The direct and transitive dependencies of each statement, without the
     statement itself. *)
  LabelMap.iter statements ~f:(fun ~key:label ~data:_ ->
      Fmt.pr "transitive_dependence_ids %d: %a@." label Test_utils.pp_labels
        (Set.Poly.remove label (transitive_dependence_ids statements label)));
  [%expect
    {|
    transitive_dependence_ids 1:
    transitive_dependence_ids 2:
    transitive_dependence_ids 3:
    transitive_dependence_ids 4:
    transitive_dependence_ids 5: 4
    transitive_dependence_ids 6: 4 5
    transitive_dependence_ids 7: 4 5
    transitive_dependence_ids 8: 4 5
    transitive_dependence_ids 9: 4 5 11 13
    transitive_dependence_ids 10: 4 5 9 11 13
    transitive_dependence_ids 11: 4 5 9 13
    transitive_dependence_ids 12: 4 5 9 11 13
    transitive_dependence_ids 13: 4 5 9 11
    transitive_dependence_ids 14: 4 5 9 11 13
    transitive_dependence_ids 15: 4 5 9 11 13 14
    transitive_dependence_ids 16: 4 5 9 11 13 14
    transitive_dependence_ids 17: 4 5 9 11 13 14 16
    transitive_dependence_ids 18: 4 5 9 11 13 14 16 17
    transitive_dependence_ids 19: 4 5 9 11 13 14 16 17
    transitive_dependence_ids 20: 4 5 9 11 13 14 16 17
    transitive_dependence_ids 21: 4 5 9 11 13 14 16 17
    transitive_dependence_ids 22: 4 5 9 11 13 14 16 17 19
    |}]

let%expect_test "Variable dependency example" =
  let statements = build_log_prob_dependence_info example1_program in
  Fmt.pr "var_dependence_ids {j} 17: %a@." Test_utils.pp_labels
    (var_dependence_ids statements (Set.Poly.singleton "j") 17);
  [%expect {| var_dependence_ids {j} 17: 4 5 9 11 13 14 16 |}]

module Accesses = Analysis_and_optimization.Dependence.Accesses

(* A definition from outside the analyzed code has no label, so [None] is a key
   distinct from the root statement at label 1. *)
let%test "External definitions are distinct from statement label 1" =
  let definition var statement_id : reaching_definition =
    {access= {var; path= []; kind= Accesses.Write}; statement_id; loop_ids= []}
  in
  let definitions =
    Set.Poly.of_list
      [definition "x" None; definition "x" (Some 1); definition "y" (Some 2)]
  in
  let labels_of_x =
    Set.Poly.filter_map definitions ~f:(fun definition ->
        if String.equal definition.access.var "x" then
          Some definition.statement_id
        else None) in
  Set.Poly.equal labels_of_x (Set.Poly.of_list [None; Some 1])

(* The data [N] reaches the condition as a definition from outside the analyzed
   code, but the dependencies hold statement labels only, so the if at label 4
   has no data dependency. *)
let%expect_test "External inputs reach a statement but are not dependencies" =
  print_immediate_dependencies
    {|
      data { int N; }
      parameters { real mu; }
      model { if (N > 0) target += mu; }
    |};
  [%expect
    {|
    1: 2 3  //
    2: real mu;  //
    3: { 4 }  //
    4: if((N > 0)) 5  //
    5: target += mu;  // data 2  control 4
    |}]

let%expect_test "Transitive dependencies of an if and of its condition" =
  let map =
    build_log_prob_dependence_info
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
      ~f:(fun ~key:statement_id ~data:statement found_id ->
        match statement.pattern with
        | Stmt.Pattern.IfElse _ -> statement_id
        | _ -> found_id) in
  Fmt.pr "%a" pp_immediate_dependencies map;
  Fmt.pr "transitive_dependence_ids %d: %a@." if_id Test_utils.pp_labels
    (transitive_dependence_ids map if_id);
  Fmt.pr "var_dependence_ids {b} %d: %a@." if_id Test_utils.pp_labels
    (var_dependence_ids map (Set.Poly.singleton "b") if_id);
  Fmt.pr "var_dependence_ids ~blockers:{b} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (var_dependence_ids map ~blockers:(Set.Poly.singleton "b")
       (Set.Poly.singleton "b") if_id);
  Fmt.pr "var_dependence_ids ~blockers:{a} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (var_dependence_ids map ~blockers:(Set.Poly.singleton "a")
       (Set.Poly.singleton "b") if_id);
  Fmt.pr "var_dependence_ids ~blockers:{mu} {b} %d: %a@." if_id
    Test_utils.pp_labels
    (var_dependence_ids map ~blockers:(Set.Poly.singleton "mu")
       (Set.Poly.singleton "b") if_id);
  (* The condition does not read c, so a query for c finds no writer, although
     two writes of c reach the condition. *)
  Fmt.pr "var_dependence_ids {c} %d: %a@." if_id Test_utils.pp_labels
    (var_dependence_ids map (Set.Poly.singleton "c") if_id);
  [%expect
    {|
    1: 2 3 4  //
    2: real mu;  //
    3: real sigma;  //
    4: { 5 6 7 8 9 10 11 }  //
    5: real a;  //
    6: a = (mu + promote(1, real, data));  // data 2
    7: real b;  //
    8: b = (a * promote(2, real, data));  // data 6
    9: real c;  //
    10: c = sigma;  // data 3
    11: if((b > 0)) 12  // data 8
    12: target += normal_lpdf(c, promote(0, real, data), promote(1, real, data));  // data 10  control 11
    transitive_dependence_ids 11: 2 6 8 11
    var_dependence_ids {b} 11: 2 6 8
    var_dependence_ids ~blockers:{b} {b} 11:
    var_dependence_ids ~blockers:{a} {b} 11: 8
    var_dependence_ids ~blockers:{mu} {b} 11: 6 8
    var_dependence_ids {c} 11:
    |}]

let%expect_test
    "Reaching definitions: an _lp call in an assignment defines target" =
  let map =
    build_log_prob_dependence_info
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
  LabelMap.iter map ~f:(fun ~key:statement_id ~data:statement ->
      match statement.pattern with
      | Stmt.Pattern.IfElse _ ->
          Fmt.pr "%d: %a  // %a@." statement_id Test_utils.pp_stmt
            statement.pattern pp_dependences statement
      | _ -> ());
  [%expect {| 6: if((target() > 0)) 7  // data 5 |}]

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
    4: FnValidateSize__("b", "N", N);  //
    5: vector[N] b;  //
    6: FnValidateSize__("c", "N", N);  //
    7: vector[N] c;  //
    8: FnValidateSize__("d", "N", N);  //
    9: vector[N] d;  //
    10: for(n in 1:(N - 1)) 11  //
    11: { 12 13 14 }  // control 10
    12: c[n] = b[(n + 1)];  // data 5  control 10
    13: d[n] = b[n];  // data 5  control 10
    14: b[n] = a;  // data 2  control 10
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
    4: FnValidateSize__("b", "N", N);  //
    5: vector[N] b;  //
    6: FnValidateSize__("c", "N", N);  //
    7: FnValidateSize__("c", "M", M);  //
    8: matrix[N, M] c;  //
    9: FnValidateSize__("d", "N", N);  //
    10: FnValidateSize__("d", "M", M);  //
    11: matrix[N, M] d;  //
    12: for(n in 2:(N - 1)) 13  //
    13: { 14 }  // control 12
    14: for(m in 1:M) 15  // control 12
    15: { 16 17 18 }  // control 14
    16: c[n, m] = b[(n + 1)];  // data 5  control 14
    17: d[n, m] = b[(n - 1)];  // data 5 18  control 14
    18: b[n] = a;  // data 2  control 14
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
    6: theta[1] = a;  // data 2
    7: theta[2] = b;  // data 3
    8: target += normal_lupdf(y, theta[1], s);  // data 5 6
    9: target += normal_lupdf(y, theta[2], s);  // data 5 7 8
    |}]

(* A read-write of target uses the latest read-write of target, and target() is
   a plain read of the same value. *)
let%expect_test "Graph: each read-write of target uses the previous one" =
  print_immediate_dependencies
    {|
      parameters { real a; real b; }
      model {
        target += a;
        target += b;
        target += target();
      }
    |};
  [%expect
    {|
    1: 2 3 4  //
    2: real a;  //
    3: real b;  //
    4: { 5 6 7 }  //
    5: target += a;  // data 2
    6: target += b;  // data 3 5
    7: target += target();  // data 6
    |}]

let%expect_test "Variables read at a set of labels" =
  let map =
    build_log_prob_dependence_info
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
  let print_used_vars_at labels =
    Fmt.pr "used_vars_at {%a}: %a@." Test_utils.pp_labels labels
      Fmt.(list ~sep:(any " ") string)
      (Set.Poly.to_list (used_vars_at map labels)) in
  Fmt.pr "%a" pp_immediate_dependencies map;
  print_used_vars_at (Set.Poly.singleton 6);
  print_used_vars_at (Set.Poly.singleton 9);
  print_used_vars_at (Set.Poly.of_list [6; 7; 9]);
  [%expect
    {|
    1: 2 3  //
    2: real mu;  //
    3: { 4 5 6 7 }  //
    4: FnValidateSize__("v", "N", N);  //
    5: vector[N] v;  //
    6: v[k] = (x[k] + mu);  // data 2
    7: for(n in 1:N) 8  //
    8: { 9 }  // control 7
    9: v[n] = x[n];  // control 7
    used_vars_at {6}: k mu x
    used_vars_at {9}: x
    used_vars_at {6 7 9}: N k mu x
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
    3: row_vector[N] r;  //
    4: { 5 6 7 8 9 10 11 12 13 14 15 17 18 19 21 22 23 24 26 27 28 30 31 32 34 35 36 }  //
    5: FnValidateSize__("theta", "N", N);  //
    6: vector[N] theta;  //
    7: FnValidateSize__("m", "N", N);  //
    8: FnValidateSize__("m", "N", N);  //
    9: matrix[N, N] m;  //
    10: tuple_ad(var, var)tuple(real, real) t;  //
    11: int j;  //
    12: j = k;  //
    13: theta[1] = a;  // data 2
    14: theta[1] = promote(1, real, var);  //
    15: if((theta[1] > 0)) 16  // data 6 14
    16: target += 1;  // control 15
    17: theta[k] = a;  // data 2
    18: theta[k] = promote(1, real, var);  //
    19: if((theta[k] > 0)) 20  // data 6 14 18
    20: target += 1;  // data 16  control 19
    21: theta[j] = a;  // data 2 12
    22: j = 2;  //
    23: theta[j] = promote(1, real, var);  // data 22
    24: if((theta[1] > 0)) 25  // data 6 14 18 21 23
    25: target += 1;  // data 16 20  control 24
    26: t.1 = a;  // data 2
    27: t.1 = promote(1, real, var);  //
    28: if((t.1 > 0)) 29  // data 10 27
    29: target += 1;  // data 16 20 25  control 28
    30: m[1, 2] = a;  // data 2
    31: m[1] = r;  // data 3
    32: if((m[1, 2] > 0)) 33  // data 9 31
    33: target += 1;  // data 16 20 25 29  control 32
    34: m[2] = r;  // data 3
    35: m[2, 2] = promote(1, real, var);  //
    36: if((m[2, 3] > 0)) 37  // data 9 31 34
    37: target += 1;  // data 16 20 25 29 33  control 36
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
    4: FnValidateSize__("theta", "N", N);  //
    5: vector[N] theta;  //
    6: theta[1] = a;  // data 2
    7: if((c > 0)) 8  //
    8: target += 1;  // control 7
    9: theta[1] = promote(1, real, var);  //
    10: if((theta[1] > 0)) 11  // data 5 9
    11: target += 1;  // data 8  control 10
    12: theta[2] = a;  // data 2
    13: if((c > 0)) 14 else 15  //
    14: theta[2] = promote(1, real, var);  // control 13
    15: theta[2] = promote(2, real, var);  // control 13
    16: if((theta[2] > 0)) 17  // data 5 14 15
    17: target += 1;  // data 8 11  control 16
    18: theta[3] = a;  // data 2
    19: if((c > 0)) 20  //
    20: theta[3] = promote(1, real, var);  // control 19
    21: if((theta[3] > 0)) 22  // data 5 18 20
    22: target += 1;  // data 8 11 17  control 21
    23: theta[1:2] = (FnMakeRowVec__(a, a) ');  // data 2
    24: theta[1:2] = (FnMakeRowVec__(promote(1, real, data), promote(1, real, data)) ');  //
    25: if((theta[1] > 0)) 26  // data 5 9 23 24
    26: target += 1;  // data 8 11 17 22  control 25
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
    4: FnValidateSize__("theta", "N", N);  //
    5: vector[N] theta;  //
    6: for(n in 2:N) 7  //
    7: { 8 9 11 12 }  // control 6
    8: theta[n] = a;  // data 2  control 6
    9: while((c > 0)) 10  // control 6 10
    10: break;  // control 9
    11: theta[n] = promote(1, real, var);  // control 6
    12: if((theta[(n - 1)] > 0)) 13  // data 5 11  control 6
    13: target += 1;  // data 13  control 12
    14: for(n in 2:N) 15  //
    15: { 16 18 19 }  // control 14
    16: if((c > 0)) 17  // control 14
    17: theta[n] = a;  // data 2  control 16
    18: theta[n] = promote(1, real, var);  // control 14
    19: if((theta[(n - 1)] > 0)) 20  // data 5 11 18  control 14
    20: target += 1;  // data 13 20  control 19
    21: for(n in 2:N) 22  //
    22: { 23 24 27 }  // control 21
    23: theta[n] = a;  // data 2  control 21
    24: if((c > 0)) 25 else 26  // control 21
    25: theta[n] = promote(1, real, var);  // control 24
    26: theta[n] = promote(2, real, var);  // control 24
    27: if((theta[(n - 1)] > 0)) 28  // data 5 11 18 23 25 26  control 21
    28: target += 1;  // data 13 20 28  control 27
    29: for(n in 2:N) 30  //
    30: { 31 32 34 }  // control 29
    31: theta[n] = a;  // data 2  control 29
    32: if((c > 0)) 33  // control 29
    33: theta[n] = promote(1, real, var);  // control 32
    34: if((theta[(n - 1)] > 0)) 35  // data 5 11 18 23 25 26 31 33  control 29
    35: target += 1;  // data 13 20 28 35  control 34
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
    4: FnValidateSize__("theta", "N", N);  //
    5: vector[N] theta;  //
    6: FnValidateSize__("y", "N", N);  //
    7: vector[N] y;  //
    8: for(n in 2:N) 9  //
    9: { 10 11 12 }  // control 8
    10: theta[n] = a;  // data 2  control 8
    11: theta[n] = promote(1, real, var);  // control 8
    12: if((theta[n] > 0)) 13  // data 5 11  control 8
    13: target += 1;  // data 13  control 12
    14: for(n in 2:N) 15  //
    15: { 16 17 20 21 }  // control 14
    16: theta[n] = a;  // data 2  control 14
    17: if((c > 0)) 18 else 19  // control 14
    18: y[n] = promote(1, real, var);  // control 17
    19: y[n] = promote(2, real, var);  // control 17
    20: theta[n] = promote(1, real, var);  // control 14
    21: if((theta[n] > 0)) 22  // data 5 11 20  control 14
    22: target += 1;  // data 13 22  control 21
    23: for(n in 2:N) 24  //
    24: { 25 26 27 }  // control 23
    25: theta[n] = promote(1, real, var);  // control 23
    26: theta[n] = a;  // data 2  control 23
    27: y[n] = theta[(n - 1)];  // data 5 11 20 26  control 23
    28: for(n in 2:N) 29  //
    29: { 30 31 33 34 }  // control 28
    30: theta[n] = a;  // data 2  control 28
    31: if((c > 0)) 32  // control 28
    32: continue;  // control 31
    33: theta[n] = promote(1, real, var);  // control 28 32
    34: if((theta[(n - 1)] > 0)) 35  // data 5 11 20 26 30 33  control 28 32
    35: target += 1;  // data 13 22 35  control 32 34
    36: for(n in 2:N) 37  //
    37: { 38 39 42 }  // control 36
    38: theta[n] = a;  // data 2  control 36
    39: for(m in 1:N) 40  // control 36
    40: { 41 }  // control 39
    41: theta[n] = promote(1, real, var);  // control 39
    42: if((theta[(n - 1)] > 0)) 43  // data 5 11 20 26 30 33 38 41  control 36
    43: target += 1;  // data 13 22 35 43  control 42
    |}]
