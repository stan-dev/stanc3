open Middle
open Common
open Analysis_and_optimization

(* ---- Pi-block code generation: emission cases the corpus does not pin ---- *)

(** The rewritten [log_prob] after partial evaluation, then the report. *)
let print_vectorized prog =
  Gensym.reset_danger_use_cautiously ();
  let mir = Loop_vectorize.vectorize_loops (Test_utils.mir_of_string prog) in
  Fmt.pr "@[<v>%a@]@."
    (Fmt.list ~sep:Fmt.cut Stmt.Located.pp)
    (Optimize.partial_evaluation mir).log_prob;
  Fmt.pr "%a"
    Fmt.(list ~sep:nop Loop_vectorize.pp_loop_report)
    (Loop_vectorize.loop_reports ())

let%expect_test "Loop vectorization: an affine offset shifts the slice" =
  print_vectorized
    {|
      data { int N; vector[N] x; }
      model {
        vector[N + 1] a;
        a[1] = 0;
        for (n in 1:N) {
          a[n + 1] = x[n];
        }
      }
    |};
  [%expect
    {|
    {
      data int a_1dim__ = (N + 1);
      FnValidateSize__("a", "N + 1", a_1dim__);
      vector[a_1dim__] a;
      a[1] = promote(0, real, var);
      a[2:(N + 1)] = x;
    }
    loop at 'string', line 6, column 8 to line 8, column 9  (n in 1:N)
      S0  a[(n + 1)] = x[n];   hoisted
      edges: none
      blocks: [S0]
    |}]

let%expect_test "Loop vectorization: a symbolic offset shifts both bounds" =
  print_vectorized
    {|
      data { int N; int k; vector[N] x; }
      model {
        vector[N + k] v;
        for (n in 1:N) {
          v[n + k] = x[n];
        }
      }
    |};
  [%expect
    {|
    {
      data int v_1dim__ = (N + k);
      FnValidateSize__("v", "N + k", v_1dim__);
      vector[v_1dim__] v;
      v[(1 + k):(N + k)] = x;
    }
    loop at 'string', line 5, column 8 to line 7, column 9  (n in 1:N)
      S0  v[(n + k)] = x[n];   hoisted
      edges: none
      blocks: [S0]
    |}]

let%expect_test "Loop vectorization: a self anti-dependence is one statement" =
  print_vectorized
    {|
      data { int N; vector[N] x; }
      model {
        vector[N] a = x;
        for (n in 1:(N - 1)) {
          a[n] = a[n + 1] + 1;
        }
      }
    |};
  [%expect
    {|
    {
      FnValidateSize__("a", "N", N);
      vector[N] a;
      a = x;
      a[1:(N - 1)] = (a[2:((N - 1) + 1)] + promote(1, real, data));
    }
    loop at 'string', line 5, column 8 to line 7, column 9  (n in 1:(N - 1))
      S0  a[n] = (a[(n + 1)] + promote(1, real, data));   hoisted
      edges: none
      blocks: [S0]
    |}]

let%expect_test "Loop vectorization: a backward anti edge reverses the order" =
  print_vectorized
    {|
      data { int N; vector[N] x; }
      model {
        vector[N + 1] a = rep_vector(0, N + 1);
        vector[N] b;
        for (n in 1:N) {
          b[n] = a[n + 1];
          a[n] = x[n];
        }
      }
    |};
  [%expect
    {|
    {
      data int a_1dim__ = (N + 1);
      FnValidateSize__("a", "N + 1", a_1dim__);
      vector[a_1dim__] a;
      a = rep_vector(promote(0, real, data), (N + 1));
      FnValidateSize__("b", "N", N);
      vector[N] b;
      b[:] = a[2:(N + 1)];
      a[1:N] = x;
    }
    loop at 'string', line 6, column 8 to line 9, column 9  (n in 1:N)
      S0  b[n] = a[(n + 1)];   hoisted
      S1  a[n] = x[n];   hoisted
      edges: S0 -> S1 a {<} d=1 (anti)
      blocks: [S0] [S1]
    |}]

let%expect_test "Loop vectorization: residual loops keep their side of an edge"
    =
  print_vectorized
    {|
      data { int N; vector[N] x; }
      model {
        vector[N] v; vector[N] w;
        for (n in 1:N) {
          v[n] = 2 * x[n];
          print(v[n]);
        }
        for (n in 1:N) {
          print(w[n]);
          w[n] = x[n];
        }
      }
    |};
  [%expect
    {|
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      FnValidateSize__("w", "N", N);
      vector[N] w;
      v[:] = (promote(2, real, data) * x);
      for(n in 1:N) FnPrint__(v[n]);
      for(n in 1:N) FnPrint__(w[n]);
      w[:] = x;
    }
    loop at 'string', line 5, column 8 to line 8, column 9  (n in 1:N)
      S0  v[n] = (promote(2, real, data) * x[n]);   hoisted
      S1  FnPrint__(v[n]);   sequential: has effects (print, reject or a user-defined function call)
      edges: S0 -> S1 v {=} d=0 (flow)
      blocks: [S0] [S1]
    loop at 'string', line 9, column 8 to line 12, column 9  (n in 1:N)
      S0  FnPrint__(w[n]);   sequential: has effects (print, reject or a user-defined function call)
      S1  w[n] = x[n];   hoisted
      edges: S0 -> S1 w {=} d=0 (anti)
      blocks: [S0] [S1]
    |}]

let%expect_test
    "Loop vectorization: a write-only scatter hoists, a read one not" =
  print_vectorized
    {|
      data { int N; vector[N] b; vector[N] x; real c; array[N] int<lower=1, upper=N> idx; }
      model {
        vector[N] a; vector[N] s; vector[N] y;
        for (n in 1:N) {
          a[idx[n]] = b[idx[n]] + c;
        }
        for (n in 1:N) {
          s[idx[n]] = x[n];
          y[n] = s[idx[n]];
        }
      }
    |};
  [%expect
    {|
    {
      FnValidateSize__("a", "N", N);
      vector[N] a;
      FnValidateSize__("s", "N", N);
      vector[N] s;
      FnValidateSize__("y", "N", N);
      vector[N] y;
      a[idx] = (b[idx] + c);
      for(n in 1:N) {
        s[idx[n]] = x[n];
        y[n] = s[idx[n]];
      }
    }
    loop at 'string', line 5, column 8 to line 7, column 9  (n in 1:N)
      S0  a[idx[n]] = (b[idx[n]] + c);   hoisted
      edges: S0 -> S0 a unknown (output)
      blocks: [S0]
    loop at 'string', line 8, column 8 to line 11, column 9  (n in 1:N)
      S0  s[idx[n]] = x[n];   sequential: in a dependence cycle with S1
      S1  y[n] = s[idx[n]];   sequential: in a dependence cycle with S0
      edges: S0 -> S0 s unknown (output); S0 -> S1 s unknown (flow); S1 -> S0 s unknown (anti)
      blocks: [S0 S1]cyclic
    |}]

let%expect_test "Loop vectorization: a profile hoists as one unit" =
  print_vectorized
    {|
      data { int N; vector[N] x; vector[N] y; real sigma; }
      model {
        vector[N] v;
        for (n in 1:N) {
          profile("mu") {
            v[n] = x[n] + 1;
          }
          target += normal_lpdf(y[n] | v[n], sigma);
        }
      }
    |};
  [%expect
    {|
    {
      FnValidateSize__("v", "N", N);
      vector[N] v;
      profile("mu"){
        v[:] = (x + promote(1, real, data));
      }
      target += normal_lpdf(y, v, sigma);
    }
    loop at 'string', line 5, column 8 to line 10, column 9  (n in 1:N)
      S0  profile("mu"){ v[n] = (x[n] + promote(1, real, data)); }   hoisted
      S1  target += normal_lpdf(y[n], v[n], sigma);   hoisted
      edges: S0 -> S1 v {=} d=0 (flow)
      blocks: [S0] [S1]
    |}]

let%expect_test "Loop vectorization: a scalar written every iteration stays" =
  print_vectorized
    {|
      data { int N; vector[N] x; }
      model {
        real t;
        for (n in 1:N) {
          t = x[n];
        }
        target += t;
      }
    |};
  [%expect
    {|
    {
      real t;
      for(n in 1:N) {
        t = x[n];
      }
      target += t;
    }
    loop at 'string', line 5, column 8 to line 7, column 9  (n in 1:N)
      S0  t = x[n];   sequential: no index varies with the loop
      edges: S0 -> S0 t unknown (output)
      blocks: [S0]
    |}]
