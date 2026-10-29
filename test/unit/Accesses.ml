(* These tests check {!Analysis_and_optimization.Accesses}. The module records
   the accesses of each statement. An access is one read, write or increment of
   a variable. The dependence analysis uses the accesses to find the earlier
   statements that a statement depends on.

   The tests build a map of statements with [log_prob_build_dep_info_map]. A
   label is the integer that identifies a statement in this map. The map covers
   the log density, which contains the statements of the parameters, transformed
   parameters and model blocks.

   The tests print [label: statement // accesses]. R marks a read, and W marks a
   write. += marks an increment of target, the log density accumulator. Each
   access prints the variable and the indices. An index with a form
   ({!Analysis_and_optimization.Index_form}) is a sum of a loop variable, a
   symbol and a constant, and prints as that sum, as in [n+k-1]. A symbol is an
   expression that has the same value in every iteration. An index without a
   loop variable, as in [v[k]] or [v[3]], selects the same element in every
   iteration. Every other index has no form and prints as ?, such as an index
   that reads a variable that a statement in the log density writes, or a loop
   variable in a form other than a sum.

   The path of an access lists the subscripts and tuple fields that select the
   element. Each subscript or tuple field is one step of the path. *)

open Std
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_types
module Accesses = Analysis_and_optimization.Accesses

let pp_node_accesses ppf (info : dependence_info) =
  LabelMap.iter info.statements ~f:(fun ~key:statement_id ~data:statement ->
      if not (List.is_empty statement.accesses) then
        Fmt.pf ppf "%d: %a  // %a@." statement_id Test_utils.pp_stmt
          statement.pattern
          (Test_utils.one_line Accesses.pp)
          statement.accesses)

let print_node_accesses prog =
  Fmt.pr "%a" pp_node_accesses
    (log_prob_build_dep_info_map (Test_utils.mir_of_string prog))

let%expect_test "Single indices: with a loop variable, without one, and no form"
    =
  print_node_accesses
    {|
      data {
        int N; int J; int k;
        vector[N] v;
        array[N] int<lower=1, upper=N> idx;
      }
      model {
        vector[N] y;
        int m = 1;
        for (n in 1:N) {
          y[n] = v[n + 1] + v[n - 2] + v[1 + n] + v[k] + v[3];
          y[n] = v[idx[n]] + v[2 * n] + v[n + k] + v[m];
          y[n] = v[k + n - 1] + v[n - k] + v[n + k + k] + v[n + k - k] + v[k + 1] + v[N - n] + v[n * k] + v[(n + 1) * 2];
          m = n;
          for (j in 1:J) y[n] = v[j];
        }
      }
    |};
  [%expect
    {|
    3: FnValidateSize__("y", "N", N);  // R N
    4: vector[N] y;  // R N, W y
    5: int m;  // W m
    6: m = 1;  // W m
    7: for(n in 1:N) 8  // R N
    9: y[n] = ((((v[(n + 1)] + v[(n - 2)]) + v[(1 + n)]) + v[k]) + v[3]);  // R v[n+1], R v[n-2], R v[n+1], R v[k], R k, R v[3], W y[n]
    10: y[n] = (((v[idx[n]] + v[(2 * n)]) + v[(n + k)]) + v[m]);  // R v[?], R idx[n], R v[?], R v[n+k], R k, R v[?], R m, W y[n]
    11: y[n] = (((((((v[((k + n) - 1)] + v[(n - k)]) + v[((n + k) + k)]) + v[((n + k) - k)]) + v[(k + 1)]) + v[(N - n)]) + v[(n * k)]) + v[((n + 1) * 2)]);  // R v[n+k-1], R k, R v[?], R k, R v[?], R k, R k, R v[n], R k, R k, R v[k+1], R k, R v[-n+N], R N, R v[?], R k, R v[?], W y[n]
    12: m = n;  // W m
    13: for(j in 1:J) 14  // R J
    15: y[n] = v[j];  // R v[j], W y[n]
    |}]

let%expect_test "Every index kind of the language" =
  print_node_accesses
    {|
      data {
        int N; int K; int a; int b;
        vector[N] v; matrix[N, K] m;
        array[N] int<lower=1, upper=N> idx;
        array[N, 2] int<lower=1, upper=N> pairs;
      }
      model {
        vector[N] y; row_vector[K] r; vector[N] c;
        for (n in 1:N) {
          y[n] = sum(v[:]) + sum(v[a:]) + sum(v[a:b]) + sum(v[:b]) + sum(v[idx]);
          y[n] = sum(v[n:]) + sum(v[n:n + 1]) + sum(v[idx[n]:N]) + sum(v[pairs[n]]);
          r = m[n, :];
          r = m[n, 1:K];
          c = m[:, 1];
          c = m[idx, 2];
          c[2:N] = y[1:(N - 1)];
        }
      }
    |};
  [%expect
    {|
    3: FnValidateSize__("y", "N", N);  // R N
    4: vector[N] y;  // R N, W y
    5: FnValidateSize__("r", "K", K);  // R K
    6: row_vector[K] r;  // R K, W r
    7: FnValidateSize__("c", "N", N);  // R N
    8: vector[N] c;  // R N, W c
    9: for(n in 1:N) 10  // R N
    11: y[n] = ((((sum(v[:]) + sum(v[a:])) + sum(v[a:b])) + sum(v[1:b])) + sum(v[idx]));  // R v[:], R v[a:], R a, R v[a:b], R a, R b, R v[1:b], R b, R v[{idx}], R idx, W y[n]
    12: y[n] = (((sum(v[n:]) + sum(v[n:(n + 1)])) + sum(v[idx[n]:N])) + sum(v[pairs[n]]));  // R v[n:], R v[n:n+1], R v[?:N], R idx[n], R N, R v[{?}], R pairs[n], W y[n]
    13: r = m[n, :];  // R m[n, :], W r
    14: r = m[n, 1:K];  // R m[n, 1:K], R K, W r
    15: c = m[:, 1];  // R m[:, 1], W c
    16: c = m[idx, 2];  // R m[{idx}, 2], R idx, W c
    17: c[2:N] = y[1:(N - 1)];  // R N, R y[1:N-1], R N, W c[2:N]
    |}]

let%expect_test "Statement kinds: declarations, target, effects and nesting" =
  print_node_accesses
    {|
      data { int N; vector[N] x; vector[N] w; }
      parameters { real mu; }
      model {
        vector[N] v;
        real acc = 0;
        for (n in 1:N) {
          real t = 2 * x[n];
          vector[2] u;
          v[n] = t + w[n];
          acc += x[n] * w[n];
          target += normal_lpdf(x[n] | v[n], mu);
          if (v[n] > 0) print(v[n]); else v[n] = sum(v);
          while (v[n] < 0) v[n] = 0;
        }
      }
    |};
  [%expect
    {|
    2: real mu;  // W mu
    4: FnValidateSize__("v", "N", N);  // R N
    5: vector[N] v;  // R N, W v
    6: real acc;  // W acc
    7: acc = promote(0, real, var);  // W acc
    8: for(n in 1:N) 9  // R N
    10: real t;  // W t
    11: t = (promote(2, real, data) * x[n]);  // R x[n], W t
    12: vector[2] u;  // W u
    13: v[n] = (t + w[n]);  // R t, R w[n], W v[n]
    14: acc = (acc + (x[n] * w[n]));  // R acc, R x[n], R w[n], W acc
    15: target += normal_lpdf(x[n], v[n], mu);  // R x[n], R v[n], R mu, += target
    16: if((v[n] > 0)) 17 else 18  // R v[n]
    17: FnPrint__(v[n]);  // R v[n]
    18: v[n] = sum(v);  // R v, W v[n]
    19: while((v[n] < 0)) 20  // R v[n]
    20: v[n] = promote(0, real, var);  // W v[n]
    |}]

let accesses_example =
  Test_utils.mir_of_string
    {|
        data {
          int N; int k;
          vector[N] x;
          array[N] int<lower=1, upper=N> idx;
        }
        parameters { real mu; }
        model {
          vector[N] v;
          vector[2] theta;
          int m = 1;
          theta[1] = mu;
          theta[2] = x[k];
          v[m] = x[m];
          for (n in 1:N) {
            v[n] = x[n + 1] + theta[1] + v[idx[n]] + v[m];
            for (j in 1:2) {
              theta[j] = v[n] + v[k] + sum(v[n:N]);
            }
          }
          target += normal_lpdf(x | v, theta[2]);
        }
      |}

let%expect_test "Nodes outside a loop and in nested loops" =
  let map = log_prob_build_dep_info_map accesses_example in
  Fmt.pr "%a" pp_node_accesses map;
  [%expect
    {|
    2: real mu;  // W mu
    4: FnValidateSize__("v", "N", N);  // R N
    5: vector[N] v;  // R N, W v
    6: vector[2] theta;  // W theta
    7: int m;  // W m
    8: m = 1;  // W m
    9: theta[1] = mu;  // R mu, W theta[1]
    10: theta[2] = x[k];  // R x[k], R k, W theta[2]
    11: v[m] = x[m];  // R m, R x[?], R m, W v[?]
    12: for(n in 1:N) 13  // R N
    14: v[n] = (((x[(n + 1)] + theta[1]) + v[idx[n]]) + v[m]);  // R x[n+1], R theta[1], R v[?], R idx[n], R v[?], R m, W v[n]
    17: theta[j] = ((v[n] + v[k]) + sum(v[n:N]));  // R v[n], R v[k], R k, R v[n:N], R N, W theta[j]
    18: target += normal_lpdf(x, v, theta[2]);  // R x, R v, R theta[2], += target
    |}]

let%expect_test "Accesses: _lp calls increment target, target() reads it" =
  print_node_accesses
    {|
      functions {
        void add_lp(real x) { target += x; }
        real twice_lp(real x) { target += x; return 2 * x; }
      }
      parameters { real mu; }
      model {
        add_lp(mu);
        real t = twice_lp(mu);
        if (target() > 0) target += 1;
      }
    |};
  [%expect
    {|
    2: real mu;  // W mu
    4: add_lp(mu);  // R mu, += target
    5: real t;  // W t
    6: t = twice_lp(mu);  // R mu, += target, W t
    7: if((target() > 0)) 8  // R target
    8: target += 1;  // += target
    |}]

let%expect_test "Accesses: nested indexing is one reference" =
  print_node_accesses
    {|
      data { int N; int K; array[N] vector[K] a; array[N, K] real b; }
      model {
        vector[K] y;
        for (n in 1:N) {
          y[1] = a[n][1] + a[n, 2] + b[n][1] + a[1:2][1][1];
        }
      }
    |};
  [%expect
    {|
    3: FnValidateSize__("y", "K", K);  // R K
    4: vector[K] y;  // R K, W y
    5: for(n in 1:N) 6  // R N
    7: y[1] = (((a[n][1] + a[n, 2]) + b[n][1]) + a[1:2][1][1]);  // R a[n, 1], R a[n, 2], R b[n, 1], R a[1:2], W y[1]
    |}]

let%expect_test "Accesses: a Stan Math _jacobian call increments target" =
  print_node_accesses
    {|
      parameters { real y; }
      transformed parameters {
        real x;
        x = lower_bound_jacobian(y, 0);
      }
    |};
  [%expect
    {|
    2: real y;  // W y
    3: real x;  // W x
    4: x = lower_bound_jacobian(y, promote(0, real, data));  // R y, += target, W x
    |}]

let%expect_test
    "Accesses: a user _jacobian call and jacobian += increment target" =
  print_node_accesses
    {|
      functions {
        real shift_jacobian(real x) { jacobian += x; return x + 1; }
      }
      parameters { real y; }
      transformed parameters {
        real x = shift_jacobian(y);
        jacobian += y;
      }
    |};
  [%expect
    {|
    2: real y;  // W y
    3: real x;  // W x
    4: x = shift_jacobian(y);  // R y, += target, W x
    5: jacobian += y;  // R y, += target
    |}]

let%expect_test "Accesses: a declaration reads the sizes in its type" =
  print_node_accesses
    {|
      data { int N; }
      model {
        int K = N + 1;
        matrix[N, K] m;
        array[K] vector[N] a;
      }
    |};
  [%expect
    {|
    3: int K;  // W K
    4: K = (N + 1);  // R N, W K
    5: FnValidateSize__("m", "N", N);  // R N
    6: FnValidateSize__("m", "K", K);  // R K
    7: matrix[N, K] m;  // R N, R K, W m
    8: FnValidateSize__("a", "K", K);  // R K
    9: FnValidateSize__("a", "N", N);  // R N
    10: array[vector[N], K] a;  // R N, R K, W a
    |}]

let%expect_test "Accesses: the remaining index, base and statement forms" =
  print_node_accesses
    {|
      data {
        int N; int k;
        vector[N] v; vector[N] w;
        array[N] int<lower=1, upper=N> idx;
        tuple(int, real) t;
        tuple(array[N] real, int) tv;
      }
      model {
        vector[N] y;
        for (n in 1:N) {
          y[n] = v[n + idx[n]] + v[k > 0 ? 1 : 2] + v[k && k];
          y[n] = v[k || k] + v[t.1] + v[n + n] + v[n + 2 - n];
          y[n] = tv.1[n] + rep_vector(0, N)[n] + (k > 0 ? v : w)[n];
          y[n] = (k > 0 || k < 0) ? 1 : 2;
          profile("inner") { y[n] = 0; }
          ;
        }
      }
    |};
  [%expect
    {|
    3: FnValidateSize__("y", "N", N);  // R N
    4: vector[N] y;  // R N, W y
    5: for(n in 1:N) 6  // R N
    7: y[n] = ((v[(n + idx[n])] + v[((k > 0) ? 1 : 2)]) + v[k && k]);  // R v[?], R idx[n], R v[((k > 0) ? 1 : 2)], R k, R v[k && k], R k, R k, W y[n]
    8: y[n] = (((v[k || k] + v[t.1]) + v[(n + n)]) + v[((n + 2) - n)]);  // R v[k || k], R k, R k, R v[t.1], R t.1, R v[?], R v[2], W y[n]
    9: y[n] = ((tv.1[n] + rep_vector(promote(0, real, data), N)[n]) + ((k > 0) ? v : w)[n]);  // R tv.1[n], R N, R k, R v, R w, W y[n]
    10: y[n] = promote(((k > 0) || (k < 0) ? 1 : 2), real, var);  // R k, R k, W y[n]
    12: y[n] = promote(0, real, var);  // W y[n]
    |}]
