(* These tests build the dependency map with [log_prob_build_dep_info_map]. This
   map covers the log density, which contains the statements of the parameters,
   transformed parameters and model blocks. The tests print [label: statement //
   accesses]. R marks a read, and W marks a write. += marks an increment of
   target, the log density accumulator. ?written marks an index that reads a
   variable that a statement in this analyzed region writes. ?unhandled marks an
   index that uses a loop variable in a form that the analysis does not
   support. *)

open Std
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_types
module Accesses = Analysis_and_optimization.Accesses

let pp_node_accesses ppf (statement_map : dep_info_map) =
  LabelMap.iter statement_map ~f:(fun ~key:statement_id ~data:(pattern, info) ->
      if not (List.is_empty info.accesses) then
        Fmt.pf ppf "%d: %a  // %a@." statement_id Test_utils.pp_stmt pattern
          (Test_utils.one_line (Accesses.pp Accesses.pp_index))
          info.accesses)

let print_node_accesses prog =
  Fmt.pr "%a" pp_node_accesses
    (log_prob_build_dep_info_map (Test_utils.mir_of_string prog))

let%expect_test "Single indices: sequential, invariant and varying" =
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
    10: y[n] = (((v[idx[n]] + v[(2 * n)]) + v[(n + k)]) + v[m]);  // R v[?unhandled], R idx[n], R v[?unhandled], R v[n+k], R k, R v[?written], R m, W y[n]
    11: y[n] = (((((((v[((k + n) - 1)] + v[(n - k)]) + v[((n + k) + k)]) + v[((n + k) - k)]) + v[(k + 1)]) + v[(N - n)]) + v[(n * k)]) + v[((n + 1) * 2)]);  // R v[n+k-1], R k, R v[?unhandled], R k, R v[?unhandled], R k, R k, R v[n], R k, R k, R v[k+1], R k, R v[-n+N], R N, R v[?unhandled], R k, R v[?unhandled], W y[n]
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
    12: y[n] = (((sum(v[n:]) + sum(v[n:(n + 1)])) + sum(v[idx[n]:N])) + sum(v[pairs[n]]));  // R v[n:], R v[n:n+1], R v[?unhandled:N], R idx[n], R N, R v[{?unhandled}], R pairs[n], W y[n]
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
    11: v[m] = x[m];  // R m, R x[?written], R m, W v[?written]
    12: for(n in 1:N) 13  // R N
    14: v[n] = (((x[(n + 1)] + theta[1]) + v[idx[n]]) + v[m]);  // R x[n+1], R theta[1], R v[?unhandled], R idx[n], R v[?written], R m, W v[n]
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
    7: y[n] = ((v[(n + idx[n])] + v[((k > 0) ? 1 : 2)]) + v[k && k]);  // R v[?unhandled], R idx[n], R v[((k > 0) ? 1 : 2)], R k, R v[k && k], R k, R k, W y[n]
    8: y[n] = (((v[k || k] + v[t.1]) + v[(n + n)]) + v[((n + 2) - n)]);  // R v[k || k], R k, R k, R v[t.1], R t.1, R v[?unhandled], R v[2], W y[n]
    9: y[n] = ((tv.1[n] + rep_vector(promote(0, real, data), N)[n]) + ((k > 0) ? v : w)[n]);  // R tv.1[n], R N, R k, R v, R w, W y[n]
    10: y[n] = promote(((k > 0) || (k < 0) ? 1 : 2), real, var);  // R k, R k, W y[n]
    12: y[n] = promote(0, real, var);  // W y[n]
    |}]

(* The path of an access lists the subscripts and tuple fields that select the
   element. This test compares the paths of the writes in the model block. It
   finds each write by its printed access. A write to [m[1]] overwrites [m[1,
   2]], because the row [m[1]] contains the element [m[1, 2]]. A write to [m[n,
   1]] overwrites itself only within one iteration of the loop over [n]. A slice
   write never overwrites, because the comparison treats a slice step as a step
   that may select a different element. [may_overlap] is false only when the
   steps at one position never select the same element. [fixed_path] returns
   [None] for a path with an index that is not a constant. *)
let%expect_test "Comparing write paths" =
  let map =
    log_prob_build_dep_info_map
      (Test_utils.mir_of_string
         {|
      data { int N; int k; }
      parameters { real a; row_vector[N] r; }
      model {
        matrix[N, N] m; tuple(real, vector[N]) t;
        for (n in 2:N) {
          m[1] = r;
          m[1, 2] = a;
          m[2, 2] = a;
          m[k, 1] = a;
          m[n, 1] = a;
          m[n - 1, 1] = a;
          m[1:2, 1] = [a, a]';
          t.2[3] = a;
        }
      }
    |})
  in
  let pp_access = Test_utils.one_line (Accesses.pp_access Accesses.pp_index) in
  let path_of printed =
    LabelMap.fold map ~init:None ~f:(fun ~key:_ ~data:(_, info) found ->
        match found with
        | Some _ -> found
        | None ->
            List.find_map info.accesses
              ~f:(fun (access : Accesses.index Accesses.access) ->
                if String.equal (Fmt.str "%a" pp_access access) printed then
                  Some access.path
                else None))
    |> Option.get in
  let must ~same_iteration killer killed =
    Fmt.pr "must_overwrite ~same_iteration:%b %s %s = %b@." same_iteration
      killer killed
      (Accesses.must_overwrite ~same_iteration ~killer:(path_of killer)
         ~killed:(path_of killed)) in
  let may written selected =
    Fmt.pr "may_overlap %s %s = %b@." written selected
      (Accesses.may_overlap (path_of written) (path_of selected)) in
  let fixed printed =
    Fmt.pr "fixed_path %s = %a@." printed
      Fmt.(
        option ~none:(any "None")
          (brackets
             (list ~sep:semi (fun ppf -> function
               | Accesses.Fixed_index i -> pf ppf "Fixed_index %d" i
               | Fixed_field f -> pf ppf "Fixed_field %d" f))))
      (Accesses.fixed_path (path_of printed)) in
  must ~same_iteration:false "W m[1]" "W m[1, 2]";
  must ~same_iteration:false "W m[1, 2]" "W m[1]";
  must ~same_iteration:false "W m[k, 1]" "W m[k, 1]";
  must ~same_iteration:false "W m[n, 1]" "W m[n, 1]";
  must ~same_iteration:true "W m[n, 1]" "W m[n, 1]";
  must ~same_iteration:true "W m[n, 1]" "W m[n-1, 1]";
  must ~same_iteration:true "W m[1:2, 1]" "W m[1:2, 1]";
  may "W m[2, 2]" "W m[1, 2]";
  may "W m[n, 1]" "W m[1, 2]";
  may "W m[n, 1]" "W m[n-1, 1]";
  may "W m[1]" "W m[1, 2]";
  may "W m[k, 1]" "W m[1, 2]";
  fixed "W t.2[3]";
  fixed "W m[k, 1]";
  [%expect
    {|
    must_overwrite ~same_iteration:false W m[1] W m[1, 2] = true
    must_overwrite ~same_iteration:false W m[1, 2] W m[1] = false
    must_overwrite ~same_iteration:false W m[k, 1] W m[k, 1] = true
    must_overwrite ~same_iteration:false W m[n, 1] W m[n, 1] = false
    must_overwrite ~same_iteration:true W m[n, 1] W m[n, 1] = true
    must_overwrite ~same_iteration:true W m[n, 1] W m[n-1, 1] = false
    must_overwrite ~same_iteration:true W m[1:2, 1] W m[1:2, 1] = false
    may_overlap W m[2, 2] W m[1, 2] = false
    may_overlap W m[n, 1] W m[1, 2] = false
    may_overlap W m[n, 1] W m[n-1, 1] = true
    may_overlap W m[1] W m[1, 2] = true
    may_overlap W m[k, 1] W m[1, 2] = false
    fixed_path W t.2[3] = [Fixed_field 2; Fixed_index 3]
    fixed_path W m[k, 1] = None
    |}]
