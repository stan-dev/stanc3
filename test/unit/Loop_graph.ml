open Analysis_and_optimization.Dependence_analysis
open Middle
open Analysis_and_optimization.Dataflow_types

(* ---- The loop dependence graph: leaves, edges and pi-blocks of one For
   ---- *)

(** One block per [For] of the model, in label order: the loop header, the
    leaves as [S0], [S1], ..., the edges and the pi-blocks. One map serves every
    loop, so the common loops of an inner loop's leaves include the outer loop.
*)
let print_loop_graphs prog =
  let statements =
    build_log_prob_dependence_info (Test_utils.mir_of_string prog) in
  LabelMap.iter statements ~f:(fun ~key:label ~data:statement ->
      match statement.pattern with
      | Stmt.Pattern.For {loopvar; lower; upper; _} ->
          Fmt.pr "loop (%s in %a:%a)@.%a" loopvar Expr.Typed.pp lower
            Expr.Typed.pp upper (pp_graph statements [])
            (build_loop_graph statements ~loop:label)
      | _ -> ())

let%expect_test "Loop graph: a chain of loop-independent flow edges" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; vector[N] y; array[N] int<lower=1, upper=2> g; }
      parameters { vector[2] a; real b; real s; }
      model {
        vector[N] muj; vector[N] mu;
        for (n in 1:N) {
          muj[n] = a[g[n]];
          mu[n] = muj[n] + b * x[n];
          target += normal_lpdf(y[n] | mu[n], s);
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  muj[n] = a[g[n]];
      S1  mu[n] = (muj[n] + (b * x[n]));
      S2  target += normal_lpdf(y[n], mu[n], s);
      edges: S0 -> S1 muj d=0 (flow); S1 -> S2 mu d=0 (flow)
      blocks: [S0] [S1] [S2]
    |}]

let%expect_test "Loop graph: a recurrence is a self flow edge with distance 1" =
  print_loop_graphs
    {|
      data { int N; vector[N] u; }
      model {
        vector[N] v;
        v[1] = 0;
        for (n in 2:N) {
          v[n] = v[n - 1] + u[n];
        }
      }
    |};
  [%expect
    {|
    loop (n in 2:N)
      S0  v[n] = (v[(n - 1)] + u[n]);
      edges: S0 -> S0 v d=1 (flow)
      blocks: [S0]cyclic
    |}]

let%expect_test "Loop graph: a pure anti self dependence is not an edge" =
  print_loop_graphs
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
    loop (n in 1:(N - 1))
      S0  a[n] = (a[(n + 1)] + promote(1, real, data));
      edges: none
      blocks: [S0]
    |}]

let%expect_test "Loop graph: a backward flow edge reverses the emission order" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; }
      model {
        vector[N] a; vector[N] b;
        b[1] = 0;
        for (n in 2:N) {
          a[n] = b[n - 1];
          b[n] = x[n];
        }
      }
    |};
  [%expect
    {|
    loop (n in 2:N)
      S0  a[n] = b[(n - 1)];
      S1  b[n] = x[n];
      edges: S1 -> S0 b d=1 (flow)
      blocks: [S1] [S0]
    |}]

let%expect_test "Loop graph: ties between ready blocks go to the earliest" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; }
      model {
        vector[N] a; vector[N] b; vector[N] c;
        b[1] = 0;
        for (n in 2:N) {
          a[n] = b[n - 1];
          c[n] = x[n];
          b[n] = x[n];
        }
      }
    |};
  [%expect
    {|
    loop (n in 2:N)
      S0  a[n] = b[(n - 1)];
      S1  c[n] = x[n];
      S2  b[n] = x[n];
      edges: S2 -> S0 b d=1 (flow)
      blocks: [S1] [S2] [S0]
    |}]

let%expect_test
    "Loop graph: a nested loop is one leaf, confused at the outer level" =
  print_loop_graphs
    {|
      data { int N; int M; }
      model {
        vector[M] a = rep_vector(1, M);
        for (n in 1:N) {
          for (m in 1:M) {
            a[m] = a[m] * 2;
          }
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  for(m in 1:M) { a[m] = (a[m] * promote(2, real, data)); }
      edges: S0 -> S0 a d=* (flow); S0 -> S0 a d=* (output)
      blocks: [S0]cyclic
    loop (m in 1:M)
      S0  a[m] = (a[m] * promote(2, real, data));
      edges: none
      blocks: [S0]
    |}]

let%expect_test "Loop graph: sibling inner loops share no dependence" =
  print_loop_graphs
    {|
      data { int N; int M; vector[M] x; vector[M] y; }
      model {
        matrix[N, M] a; matrix[N, M] b;
        for (n in 1:N) {
          for (m in 1:M) {
            a[n, m] = x[m];
          }
          for (m in 1:M) {
            b[n, m] = y[m];
          }
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  for(m in 1:M) { a[n, m] = x[m]; }
      S1  for(m in 1:M) { b[n, m] = y[m]; }
      edges: none
      blocks: [S0] [S1]
    loop (m in 1:M)
      S0  a[n, m] = x[m];
      edges: none
      blocks: [S0]
    loop (m in 1:M)
      S0  b[n, m] = y[m];
      edges: none
      blocks: [S0]
    |}]

(* target is an accumulator of the body: every statement that touches it only
   adds to it, so the additions can run in any order and have no edges. *)
let%expect_test "Loop graph: accumulations into target commute" =
  print_loop_graphs
    {|
      data { int N; vector[N] a; vector[N] b; }
      model {
        for (n in 1:N) {
          target += a[n];
          target += b[n];
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  target += a[n];
      S1  target += b[n];
      edges: none
      blocks: [S0] [S1]
    |}]

(* A read of the accumulator needs the sum so far, so the order is fixed and
   every edge on target stays. *)
let%expect_test "Loop graph: a read of the accumulator keeps the edges" =
  print_loop_graphs
    {|
      data { int N; vector[N] a; }
      model {
        vector[N] t;
        for (n in 1:N) {
          target += a[n];
          t[n] = target();
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  target += a[n];
      S1  t[n] = target();
      edges: S0 -> S0 target d=* (flow); S0 -> S0 target d=* (output); S0 -> S1 target d=* (flow); S1 -> S0 target d=* (anti)
      blocks: [S0 S1]cyclic
    |}]

(* A scalar sum is an accumulator too, so the self dependence through s is not
   an edge; t is not, because t = a[n] - t is not a sum into t. *)
let%expect_test "Loop graph: a scalar sum is an accumulator" =
  print_loop_graphs
    {|
      data { int N; vector[N] a; }
      model {
        real s = 0;
        real t = 1;
        for (n in 1:N) {
          s = s + a[n];
          t = a[n] - t;
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  s = (s + a[n]);
      S1  t = (a[n] - t);
      edges: S1 -> S1 t d=* (flow); S1 -> S1 t d=* (output)
      blocks: [S0] [S1]cyclic
    |}]

let%expect_test "Loop graph: two effectful statements depend on each other" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; }
      model {
        for (n in 1:N) {
          print(n);
          print(x[n]);
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  FnPrint__(n);
      S1  FnPrint__(x[n]);
      edges: S0 -> S1 (effects); S1 -> S0 (effects)
      blocks: [S0 S1]cyclic
    |}]

let%expect_test "Loop graph: an if is one leaf with both branches' accesses" =
  print_loop_graphs
    {|
      data { int N; vector[N] c; vector[N] x; }
      model {
        vector[N] v; vector[N] w; vector[N] y;
        w[1] = 0;
        for (n in 2:N) {
          if (c[n] > 0) v[n] = x[n]; else w[n] = x[n];
          y[n] = v[n] + w[n - 1];
        }
      }
    |};
  [%expect
    {|
    loop (n in 2:N)
      S0  if((c[n] > 0)) v[n] = x[n]; else w[n] = x[n];
      S1  y[n] = (v[n] + w[(n - 1)]);
      edges: S0 -> S1 v d=0 (flow); S0 -> S1 w d=1 (flow)
      blocks: [S0] [S1]
    |}]

let%expect_test "Loop graph: a scalar temporary in the body orders everything" =
  print_loop_graphs
    {|
      data { int N; vector[N] a; }
      model {
        vector[N] b;
        for (n in 1:N) {
          real t = a[n];
          b[n] = t;
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  real t;
      S1  t = a[n];
      S2  b[n] = t;
      edges: S0 -> S0 t d=* (output); S0 -> S1 t d=* (output); S1 -> S0 t d=* (output); S1 -> S1 t d=* (output); S1 -> S2 t d=* (flow); S2 -> S0 t d=* (anti); S2 -> S1 t d=* (anti)
      blocks: [S0 S1 S2]cyclic
    |}]

let%expect_test "Loop graph: the join keeps both directions, drops the distance"
    =
  print_loop_graphs
    {|
      data { int N; vector[N] x; }
      model {
        vector[N] v; vector[N] y;
        v[1] = 0;
        for (n in 2:N) {
          v[n] = x[n];
          y[n] = v[n] + v[n - 1];
        }
      }
    |};
  [%expect
    {|
    loop (n in 2:N)
      S0  v[n] = x[n];
      S1  y[n] = (v[n] + v[(n - 1)]);
      edges: S0 -> S1 v d=* (flow)
      blocks: [S0] [S1]
    |}]

let%expect_test
    "Loop graph: distinct constants are independent, a scatter is not" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; array[N] int<lower=1, upper=N> idx; }
      model {
        matrix[N, 2] m; vector[N] w; vector[N] v;
        for (n in 1:N) {
          m[n, 1] = x[n];
          w[n] = m[n, 2];
          v[idx[n]] = x[n];
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  m[n, 1] = x[n];
      S1  w[n] = m[n, 2];
      S2  v[idx[n]] = x[n];
      edges: S2 -> S2 v d=* (output)
      blocks: [S0] [S1] [S2]
    |}]

let%expect_test "Loop graph: a gather only reads" =
  print_loop_graphs
    {|
      data { int N; vector[N] b; array[N] int<lower=1, upper=N> idx; }
      model {
        vector[N] a;
        for (n in 1:N) {
          a[n] = b[idx[n]];
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  a[n] = b[idx[n]];
      edges: none
      blocks: [S0]
    |}]

let%expect_test
    "Loop graph: a pair carried by the outer loop is invisible inside" =
  print_loop_graphs
    {|
      data { int N; int M; vector[M] x; }
      model {
        matrix[N, M] b; matrix[N, M] c;
        b[1] = x';
        for (n in 2:N) {
          for (m in 1:M) {
            c[n, m] = b[n - 1, m];
            b[n, m] = x[m];
          }
        }
      }
    |};
  [%expect
    {|
    loop (n in 2:N)
      S0  for(m in 1:M) { c[n, m] = b[(n - 1), m]; b[n, m] = x[m]; }
      edges: S0 -> S0 b d=1 (flow)
      blocks: [S0]cyclic
    loop (m in 1:M)
      S0  c[n, m] = b[(n - 1), m];
      S1  b[n, m] = x[m];
      edges: none
      blocks: [S0] [S1]
    |}]

let%expect_test "Loop graph: a profile is one leaf and keeps a self anti edge" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; }
      model {
        vector[N] a; vector[N] b;
        for (n in 1:(N - 1)) {
          profile("p") {
            a[n] = x[n];
            b[n] = a[n + 1];
          }
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:(N - 1))
      S0  profile("p"){ a[n] = x[n]; b[n] = a[(n + 1)]; }
      edges: S0 -> S0 a d=1 (anti)
      blocks: [S0]cyclic
    |}]

let%expect_test "Loop graph: a declaring scope is one leaf" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; vector[N] y; }
      model {
        vector[N] v; vector[N] w;
        for (n in 1:N) {
          { real t; t = x[n]; v[n] = t; }
          { real t; t = y[n]; w[n] = t; }
        }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
      S0  { real t; t = x[n]; v[n] = t; }
      S1  { real t; t = y[n]; w[n] = t; }
      edges: S0 -> S0 t d=* (flow); S0 -> S0 t d=* (anti); S0 -> S0 t d=* (output); S0 -> S1 t d=* (flow); S0 -> S1 t d=* (anti); S0 -> S1 t d=* (output); S1 -> S0 t d=* (flow); S1 -> S0 t d=* (anti); S1 -> S0 t d=* (output); S1 -> S1 t d=* (flow); S1 -> S1 t d=* (anti); S1 -> S1 t d=* (output)
      blocks: [S0 S1]cyclic
    |}]
