open Std
open Analysis_and_optimization.Loop_dependence
open Middle
open Analysis_and_optimization.Dataflow_types

(* ---- Loop access model (L1) and dependence test (L2) ---- *)

let first_for_in_log_prob (mir : Program.Typed.t) =
  let rec find (s : Stmt.Located.t) =
    match s.pattern with
    | For {loopvar; body; _} -> Some (loopvar, body)
    | Block l | SList l | Profile (_, l) -> List.find_map l ~f:find
    | IfElse (_, a, b) -> (
        match find a with Some x -> Some x | None -> Option.bind b ~f:find)
    | While (_, b) -> find b
    | Assignment _ | TargetPE _ | JacobianPE _ | NRFunApp _ | Break | Continue
     |Return _ | Skip | Decl _ ->
        None in
  List.find_map mir.log_prob ~f:find |> Option.get

(** Print the written set of the first loop in the model block and the accesses
    of each of its leaf statements. *)
let one_line pp x =
  let b = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer b in
  Format.pp_set_margin ppf 100_000;
  pp ppf x;
  Format.pp_print_flush ppf ();
  Buffer.contents b

let print_loop_accesses prog =
  let loopvar, body = first_for_in_log_prob (Test_utils.mir_of_string prog) in
  let written_vars = Stmt.Helpers.assigned_or_declared_variables body in
  Fmt.pr "loopvar: %s@.written: %a@." loopvar
    Fmt.(list ~sep:(any ", ") string)
    (Set.Poly.to_list written_vars);
  let leaves = match body.pattern with Block l | SList l -> l | _ -> [body] in
  List.iteri leaves ~f:(fun i (s : Stmt.Located.t) ->
      Fmt.pr "%d: %s@.   %s@." i
        (one_line Stmt.Located.pp s)
        (one_line
           Fmt.(list ~sep:(any ", ") pp_access)
           (stmt_accesses ~loopvar ~written_vars ~label:i s.pattern)))

let%expect_test "classify_subscript: affine, invariant and varying indices" =
  print_loop_accesses
    {|
      data {
        int N; int J; int k; int a; int b;
        vector[N] v;
        array[N] int<lower=1, upper=N> idx;
      }
      model {
        vector[N] y;
        int m = 1;
        for (n in 1:N) {
          y[n] = v[n + 1] + v[n - 2] + v[1 + n] + v[k] + v[3];
          y[n] = v[idx[n]] + v[2 * n] + v[n + k] + sum(v[:]) + sum(v[a:b]) + v[m];
          y[n] = v[k + n - 1] + v[n - k] + v[n + k + k] + v[n + k - k] + v[k + 1] + v[N - n] + v[n * k] + v[(n + 1) * 2];
          m = n;
          for (j in 1:J) y[n] = v[j];
        }
      }
    |};
  [%expect
    {|
    loopvar: n
    written: j, m, y
    0: y[n] = ((((v[(n + 1)] + v[(n - 2)]) + v[(1 + n)]) + v[k]) + v[3]);
       R v[i+1], R v[i-2], R v[i+1], R v[k], R k, R v[3], W y[i]
    1: y[n] = (((((v[idx[n]] + v[(2 * n)]) + v[(n + k)]) + sum(v[:])) + sum(v[a:b])) + v[m]);
       R v[?gather], R idx[i], R v[2i], R v[i+k], R k, R v[?slice], R v[?slice], R a, R b, R v[?written], R m, W y[i]
    2: y[n] = (((((((v[((k + n) - 1)] + v[(n - k)]) + v[((n + k) + k)]) + v[((n + k) - k)]) + v[(k + 1)]) + v[(N - n)]) + v[(n * k)]) + v[((n + 1) * 2)]);
       R v[i+k-1], R k, R v[i-k], R k, R v[i+2*k], R k, R k, R v[i], R k, R k, R v[k+1], R k, R v[-i+N], R N, R v[?nonlinear], R k, R v[2i+2], W y[i]
    3: m = n;
       W m
    4: for(j in 1:J) { y[n] = v[j];
    }
       W j, R J, R v[?written], R j, W y[i]
    |}]

let%expect_test "stmt_accesses: declarations, target, effects and nesting" =
  print_loop_accesses
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
    loopvar: n
    written: acc, t, u, v
    0: real t;
       W t
    1: t = (promote(2, real, data) * x[n]);
       R x[i], W t
    2: vector[2] u;
       W u
    3: v[n] = (t + w[n]);
       R t, R w[i], W v[i]
    4: acc = (acc + (x[n] * w[n]));
       R x[i], R w[i], += acc
    5: target += normal_lpdf(x[n], v[n], mu);
       R x[i], R v[i], R mu, += target
    6: if((v[n] > 0)) FnPrint__(v[n]); else v[n] = sum(v);
       R v[i], R v[i], R v, W v[i]
    7: while((v[n] < 0)) v[n] = promote(0, real, var);
       R v[i], W v[i]
    |}]

(* ---- Loop dependence graph and pi-blocks (L3) ---- *)

(** Print the dependence graph and pi-blocks of every loop in the model block,
    in program order. *)
let print_loop_graphs prog =
  let mir = Test_utils.mir_of_string prog in
  let rec loops (s : Stmt.Located.t) =
    match s.pattern with
    | For {loopvar; body; lower; upper} ->
        Fmt.pr "loop (%s in %a:%a)@.%a@." loopvar Expr.Typed.pp lower
          Expr.Typed.pp upper pp_loop_dependence_graph
          (loop_dependence_graph ~loopvar body);
        loops body
    | Block l | SList l | Profile (_, l) -> List.iter l ~f:loops
    | IfElse (_, a, b) ->
        loops a;
        Option.iter b ~f:loops
    | While (_, b) -> loops b
    | Assignment _ | TargetPE _ | JacobianPE _ | NRFunApp _ | Break | Continue
     |Return _ | Skip | Decl _ ->
        () in
  List.iter mir.log_prob ~f:loops

let%expect_test "loop graph: design examples 1 to 4" =
  print_loop_graphs
    {|
      data {
        int N; int J;
        array[N] int<lower=1, upper=J> county_idx;
        vector[N] log_uppm; vector[N] floor_measure; vector[N] log_radon;
        vector[N] x;
      }
      parameters { vector[J] alpha; vector[2] beta; real sigma_y; }
      model {
        vector[N] mu; vector[N] muj; vector[N] v;
        for (n in 1:N) {
          muj[n] = alpha[county_idx[n]] + log_uppm[n] * beta[1];
          mu[n] = muj[n] + floor_measure[n] * beta[2];
          target += normal_lpdf(log_radon[n] | mu[n], sigma_y);
        }
        for (n in 1:N) { mu[n] = v[n] + 1; v[n] = x[n]; }
        for (n in 1:N) { mu[n] = 2 * x[n]; print(mu[n]); }
        for (n in 1:N) { print(v[n]); v[n] = x[n]; }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
    S0  muj[n] = (alpha[county_idx[n]] + (log_uppm[n] * beta[1]));
    S1  mu[n] = (muj[n] + (floor_measure[n] * beta[2]));
    S2  target += normal_lpdf(log_radon[n], mu[n], sigma_y);
    edges: S0 -> S1 muj {=} d=0 (true); S1 -> S2 mu {=} d=0 (true)
    blocks: [S0] [S1] [S2]
    loop (n in 1:N)
    S0  mu[n] = (v[n] + promote(1, real, data));
    S1  v[n] = x[n];
    edges: S0 -> S1 v {=} d=0 (anti)
    blocks: [S0] [S1]
    loop (n in 1:N)
    S0  mu[n] = (promote(2, real, data) * x[n]);
    S1  FnPrint__(mu[n]);
    edges: S0 -> S1 mu {=} d=0 (true)
    blocks: [S0] [S1]
    loop (n in 1:N)
    S0  FnPrint__(v[n]);
    S1  v[n] = x[n];
    edges: S0 -> S1 v {=} d=0 (anti)
    blocks: [S0] [S1]
    |}]

let%expect_test "loop graph: design examples 5 to 8" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; vector[N] u; }
      model {
        vector[N] mu; vector[N] v; vector[N] y;
        for (n in 1:N) { print(v[n]); v[n] = x[n]; print(v[n]); }
        for (n in 2:N) { v[n] = v[n - 1] + u[n]; }
        for (n in 2:N) { v[n] = x[n]; y[n] = v[n - 1]; }
        for (n in 1:N) { real t = 2 * x[n]; mu[n] = t + 1; }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
    S0  FnPrint__(v[n]);
    S1  v[n] = x[n];
    S2  FnPrint__(v[n]);
    edges: S0 -> S1 v {=} d=0 (anti); S0 -> S2 (effects); S1 -> S2 v {=} d=0 (true); S2 -> S0 (effects)
    blocks: [S0 S1 S2]cyclic
    loop (n in 2:N)
    S0  v[n] = (v[(n - 1)] + u[n]);
    edges: S0 -> S0 v {<} d=1 (true)
    blocks: [S0]cyclic
    loop (n in 2:N)
    S0  v[n] = x[n];
    S1  y[n] = v[(n - 1)];
    edges: S0 -> S1 v {<} d=1 (true)
    blocks: [S0] [S1]
    loop (n in 1:N)
    S0  real t;
    S1  t = (promote(2, real, data) * x[n]);
    S2  mu[n] = (t + promote(1, real, data));
    edges: S0 -> S1 t {<,=,>} (output); S0 -> S2 t {<,=,>} (true); S1 -> S0 t {<,=,>} (output); S1 -> S2 t {<,=,>} (true); S2 -> S0 t {<,=,>} (anti); S2 -> S1 t {<,=,>} (anti)
    blocks: [S0 S1 S2]cyclic
    |}]

let%expect_test "loop graph: design examples 9 to 12" =
  print_loop_graphs
    {|
      data { int N; int k; vector[N] x; array[N] int<lower=1, upper=N> idx; }
      model {
        vector[N] v; vector[N] y; vector[N] w; matrix[N, 2] m; vector[N] a;
        vector[N + k] z;
        for (n in 1:N) { v[n] = x[n]; target += sum(v); }
        for (n in 1:N) { v[idx[n]] = x[n]; y[n] = v[idx[n]]; }
        for (n in 1:N) { vector[2] t; t[1] = x[n]; y[n] = t[1]; }
        for (n in 1:N) { m[n, 1] = x[n]; w[n] = m[n, 2]; }
        for (n in 1:(N - 1)) { a[n] = a[n + 1] + 1; }
        for (n in 1:N) { z[n + k] = x[n]; y[n] = z[n + k] * 2; }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
    S0  v[n] = x[n];
    S1  target += sum(v);
    edges: S0 -> S1 v {<,=,>} (true); S1 -> S0 v {<,=,>} (anti)
    blocks: [S0 S1]cyclic
    loop (n in 1:N)
    S0  v[idx[n]] = x[n];
    S1  y[n] = v[idx[n]];
    edges: S0 -> S1 v {<,=,>} (true); S1 -> S0 v {<,=,>} (anti)
    blocks: [S0 S1]cyclic
    loop (n in 1:N)
    S0  vector[2] t;
    S1  t[1] = x[n];
    S2  y[n] = t[1];
    edges: S0 -> S1 t {<,=,>} (output); S0 -> S2 t {<,=,>} (true); S1 -> S0 t {<,=,>} (output); S1 -> S2 t {<,=,>} (true); S2 -> S0 t {<,=,>} (anti); S2 -> S1 t {<,=,>} (anti)
    blocks: [S0 S1 S2]cyclic
    loop (n in 1:N)
    S0  m[n, 1] = x[n];
    S1  w[n] = m[n, 2];
    edges: none
    blocks: [S0] [S1]
    loop (n in 1:(N - 1))
    S0  a[n] = (a[(n + 1)] + promote(1, real, data));
    edges: none
    blocks: [S0]
    loop (n in 1:N)
    S0  z[(n + k)] = x[n];
    S1  y[n] = (z[(n + k)] * promote(2, real, data));
    edges: S0 -> S1 z {=} d=0 (true)
    blocks: [S0] [S1]
    |}]

let%expect_test "loop graph: reordering and fusion cases from the corpus" =
  print_loop_graphs
    {|
      data { int N; vector[N] x; vector[N] z; vector[N] c; vector[N] d; }
      model {
        vector[N + 1] ia; vector[N + 1] ib; vector[N + 1] a; vector[N] b = x;
        // GCC f2: anti S1 -> S0 {<} 1, acyclic: S1 first
        for (n in 1:N) { ia[n] = x[n]; ib[n] = ia[n + 1]; }
        // GCC f6: output S1 -> S0
        for (n in 1:N) { ia[n] = x[n]; ia[n + 1] = z[n]; }
        // TSVC s212: anti S1 -> S0
        for (n in 1:N) { a[n] = a[n] .* c[n]; b[n] = b[n] + a[n + 1] .* d[n]; }
        // TSVC s241: cycle through true {=} and anti {<}
        for (n in 1:N) { a[n] = b[n] .* c[n] .* d[n]; b[n] = a[n] .* a[n + 1] .* d[n]; }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
    S0  ia[n] = x[n];
    S1  ib[n] = ia[(n + 1)];
    edges: S1 -> S0 ia {>} d=-1 (anti)
    blocks: [S1] [S0]
    loop (n in 1:N)
    S0  ia[n] = x[n];
    S1  ia[(n + 1)] = z[n];
    edges: S1 -> S0 ia {>} d=-1 (output)
    blocks: [S1] [S0]
    loop (n in 1:N)
    S0  a[n] = (a[n] .* c[n]);
    S1  b[n] = (b[n] + (a[(n + 1)] .* d[n]));
    edges: S1 -> S0 a {>} d=-1 (anti)
    blocks: [S1] [S0]
    loop (n in 1:N)
    S0  a[n] = ((b[n] .* c[n]) .* d[n]);
    S1  b[n] = ((a[n] .* a[(n + 1)]) .* d[n]);
    edges: S0 -> S1 a {=} d=0 (true); S0 -> S1 b {=} d=0 (anti); S1 -> S0 a {>} d=-1 (anti)
    blocks: [S0 S1]cyclic
    |}]

(* Direct tests of the dependence test, one per row of the §7.4 table. *)
let aff ?(coeff = 1) ?(terms = []) const = Affine {coeff; offset= {const; terms}}
let inv ?(terms = []) const = Invariant {const; terms}
let k = [(1, Expr.Helpers.variable "k")]
let m = [(1, Expr.Helpers.variable "m")]
let w subs = {var= "v"; subs; kind= Write; label= 0}
let r subs = {var= "v"; subs; kind= Read; label= 1}

let dep a b =
  Fmt.pr "%a  /  %a  ->  %a@." pp_access a pp_access b pp_dependence
    (access_dependence a b)

let%expect_test "access_dependence: strong SIV rows" =
  dep (w [aff 0]) (r [aff 0]);
  dep (w [aff 1]) (r [aff 0]);
  dep (w [aff 0]) (r [aff 1]);
  dep (w [aff 8]) (r [aff 0]);
  dep (w [aff ~coeff:(-1) 0]) (r [aff ~coeff:(-1) 2]);
  dep (w [aff ~coeff:2 1]) (r [aff ~coeff:2 0]);
  [%expect
    {|
    W v[i]  /  R v[i]  ->  {=} d=0
    W v[i+1]  /  R v[i]  ->  {<} d=1
    W v[i]  /  R v[i+1]  ->  {>} d=-1
    W v[i+8]  /  R v[i]  ->  {<} d=8
    W v[-i]  /  R v[-i+2]  ->  {<} d=2
    W v[2i+1]  /  R v[2i]  ->  independent
    |}]

let%expect_test "access_dependence: weak SIV, ZIV and confused rows" =
  dep (w [aff ~coeff:2 0]) (r [aff 0]);
  dep (w [aff 0]) (r [inv 1]);
  dep (w [inv 1]) (r [inv 2]);
  dep (w [inv 1]) (r [inv 1]);
  dep (w [inv ~terms:k 0]) (r [inv 1]);
  dep (w [Varying Gather]) (r [aff 0]);
  dep (w [aff 0]) (r [aff 0; inv 1]);
  dep (w []) (r []);
  dep (w []) (r [aff 0]);
  [%expect
    {|
    W v[2i]  /  R v[i]  ->  {<,=,>}
    W v[i]  /  R v[1]  ->  {<,=,>}
    W v[1]  /  R v[2]  ->  independent
    W v[1]  /  R v[1]  ->  {<,=,>}
    W v[k]  /  R v[1]  ->  {<,=,>}
    W v[?gather]  /  R v[i]  ->  {<,=,>}
    W v[i]  /  R v[i, 1]  ->  {<,=,>}
    W v  /  R v  ->  {<,=,>}
    W v  /  R v[i]  ->  {<,=,>}
    |}]

let%expect_test "access_dependence: symbolic offsets" =
  dep (w [aff ~terms:k 0]) (r [aff ~terms:k 0]);
  dep (w [aff ~terms:k 1]) (r [aff ~terms:k 0]);
  dep (w [aff ~terms:k 0]) (r [aff 0]);
  dep (w [aff ~terms:k 0]) (r [aff ~terms:m 0]);
  dep (w [inv ~terms:k 1]) (r [inv ~terms:k 2]);
  dep (w [inv ~terms:k 0]) (r [inv ~terms:k 0]);
  dep (w [inv ~terms:k 0]) (r [inv ~terms:m 0]);
  [%expect
    {|
    W v[i+k]  /  R v[i+k]  ->  {=} d=0
    W v[i+k+1]  /  R v[i+k]  ->  {<} d=1
    W v[i+k]  /  R v[i]  ->  {<,=,>}
    W v[i+k]  /  R v[i+m]  ->  {<,=,>}
    W v[k+1]  /  R v[k+2]  ->  independent
    W v[k]  /  R v[k]  ->  {<,=,>}
    W v[k]  /  R v[m]  ->  {<,=,>}
    |}]

let%expect_test "access_dependence: merging separable positions" =
  dep (w [aff 0; inv 1]) (r [aff 0; inv 2]);
  dep (w [aff 0; aff 1]) (r [aff 0; aff 0]);
  dep (w [aff 0; inv 1]) (r [inv 2; aff 0]);
  dep (w [aff 1; aff 0]) (r [aff 0; Varying Slice]);
  dep (w [aff 0; inv 1]) (r [aff 0; inv 1]);
  dep (w [aff 1; aff 1]) (r [aff 0; aff 0]);
  [%expect
    {|
    W v[i, 1]  /  R v[i, 2]  ->  independent
    W v[i, i+1]  /  R v[i, i]  ->  independent
    W v[i, 1]  /  R v[2, i]  ->  {<,=,>}
    W v[i+1, i]  /  R v[i, ?slice]  ->  {<} d=1
    W v[i, 1]  /  R v[i, 1]  ->  {=} d=0
    W v[i+1, i+1]  /  R v[i, i]  ->  {<} d=1
    |}]

let%expect_test "loop graph: scalar reductions" =
  print_loop_graphs
    {|
      data { int N; vector[N] a; vector[N] b; vector[N] y; real sigma; }
      parameters { real mu; }
      model {
        real s = 0; real lp = 0; real t = 0; real u = 1; vector[N] z; int c = 0;
        for (n in 1:N) { s += a[n]; s += b[n]; }
        for (n in 1:N) { lp += normal_lpdf(y[n] | mu, sigma); c += 1; }
        for (n in 1:N) { t += a[n]; z[n] = t; }
        for (n in 1:N) { u = u + u * a[n]; }
        for (n in 1:N) { target += normal_lpdf(y[n] | mu, sigma); s += target(); }
      }
    |};
  [%expect
    {|
    loop (n in 1:N)
    S0  s = (s + a[n]);
    S1  s = (s + b[n]);
    edges: none
    blocks: [S0] [S1]
    loop (n in 1:N)
    S0  lp = (lp + normal_lpdf(y[n], mu, sigma));
    S1  c = (c + 1);
    edges: none
    blocks: [S0] [S1]
    loop (n in 1:N)
    S0  t = (t + a[n]);
    S1  z[n] = t;
    edges: S0 -> S1 t {<,=,>} (true); S1 -> S0 t {<,=,>} (anti)
    blocks: [S0 S1]cyclic
    loop (n in 1:N)
    S0  u = (u + (u * a[n]));
    edges: S0 -> S0 u {<,=,>} (true)
    blocks: [S0]cyclic
    loop (n in 1:N)
    S0  target += normal_lpdf(y[n], mu, sigma);
    S1  s = (s + target());
    edges: S0 -> S1 target {<,=,>} (true); S1 -> S0 target {<,=,>} (anti)
    blocks: [S0 S1]cyclic
    |}]
