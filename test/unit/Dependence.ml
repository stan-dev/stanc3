(* Checks element comparisons and execution order for reaching definitions. Each
   read statement precedes its candidate source/sink pairs. For example:

   13: c[n] = b[(n - 1)]; 12 W b[n] --> R b[n-1]: constrained [n:1] flows

   Statement 12 writes the element read by statement 13 one iteration later.
   Distances print outermost first; * means unknown. Pairs are printed when
   names and kinds permit flow, even if later checks reject the pair. External
   definitions have no access record and are not printed. *)

open Std
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_types
module Accesses = Analysis_and_optimization.Accesses
module Dependence = Analysis_and_optimization.Dependence

let print_verdicts ~var prog =
  let map = log_prob_build_dep_info_map (Test_utils.mir_of_string prog) in
  let accesses_of statement_id =
    Accesses.of_var var (snd (LabelMap.find statement_id map)).accesses in
  let pp_access = Test_utils.one_line (Accesses.pp_access Accesses.pp_index) in
  LabelMap.iter map ~f:(fun ~key:sink_id ~data:(pattern, sink_info) ->
      let sinks =
        List.filter (accesses_of sink_id) ~f:(fun sink ->
            match sink.Accesses.kind with
            | Accesses.Read | Increment -> true
            | Write -> false) in
      if not (List.is_empty sinks) then (
        Fmt.pr "%d: %a@." sink_id Test_utils.pp_stmt pattern;
        let reaching_ids =
          reaching_defn_lookup sink_info.reaching_defn_entry var in
        LabelMap.iter map ~f:(fun ~key:source_id ~data:(_, source_info) ->
            if Set.Poly.mem (Some source_id) reaching_ids then
              let loops =
                Dependence.common_loops source_info.loops sink_info.loops in
              List.iter (accesses_of source_id) ~f:(fun source ->
                  List.iter sinks ~f:(fun sink ->
                      if flows_into source sink then
                        let dependence =
                          Dependence.compare_paths ~loops source sink in
                        let outcome =
                          match dependence with
                          | Dependence.Independent -> ""
                          | Unknown | Constrained _ ->
                              if
                                Dependence.may_exist
                                  ~source_first_in_text:(source_id < sink_id)
                                  dependence
                              then "  flows"
                              else "  cannot flow" in
                        Fmt.pr "  %d %a --> %a: %a%s@." source_id pp_access
                          source pp_access sink Dependence.pp dependence outcome)))))

let%expect_test "Strong SIV: loop-carried and loop-independent" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d;
        for (n in 2:N) {
          b[n] = a;
          c[n] = b[n - 1];   // loop-carried: the read is one iteration after the write
          d[n] = b[n];       // loop-independent: the same iteration, the write first
        }
      }
    |};
  [%expect
    {|
    13: c[n] = b[(n - 1)];
      5 W b --> R b[n-1]: unknown  flows
      12 W b[n] --> R b[n-1]: constrained [n:1]  flows
    14: d[n] = b[n];
      5 W b --> R b[n]: unknown  flows
      12 W b[n] --> R b[n]: constrained [n:0]  flows
    |}]

let%expect_test "Strong SIV: self pairs" =
  print_verdicts ~var:"x"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] x;
        for (n in 2:N) {
          x[n] = x[n - 1] + x[n] + a;   // x[n - 1]: flows; x[n]: the write runs after the read
        }
      }
    |};
  [%expect
    {|
    8: x[n] = ((x[(n - 1)] + x[n]) + a);
      5 W x --> R x[n-1]: unknown  flows
      5 W x --> R x[n]: unknown  flows
      8 W x[n] --> R x[n-1]: constrained [n:1]  flows
      8 W x[n] --> R x[n]: constrained [n:0]  cannot flow
    |}]

let%expect_test "Sink runs first" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d;
        for (n in 1:(N - 1)) {
          c[n] = b[n + 1];   // the element b[n] writes in the next iteration
          d[n] = b[n];       // the element b[n] writes later in this iteration
          b[n] = a;
        }
      }
    |};
  [%expect
    {|
    12: c[n] = b[(n + 1)];
      5 W b --> R b[n+1]: unknown  flows
      14 W b[n] --> R b[n+1]: constrained [n:-1]  cannot flow
    13: d[n] = b[n];
      5 W b --> R b[n]: unknown  flows
      14 W b[n] --> R b[n]: constrained [n:0]  cannot flow
    |}]

let%expect_test "ZIV: literal constants" =
  print_verdicts ~var:"theta"
    {|
      data { real y; real s; }
      parameters { real a; real b; }
      model {
        vector[2] theta;
        theta[1] = a;
        theta[2] = b;
        y ~ normal(theta[1], s);   // theta[2] = b never touches theta[1]
        y ~ normal(theta[2], s);
      }
    |};
  [%expect
    {|
    8: target += normal_lupdf(y, theta[1], s);
      5 W theta --> R theta[1]: unknown  flows
      6 W theta[1] --> R theta[1]: unknown  flows
      7 W theta[2] --> R theta[1]: independent
    9: target += normal_lupdf(y, theta[2], s);
      5 W theta --> R theta[2]: unknown  flows
      6 W theta[1] --> R theta[2]: independent
      7 W theta[2] --> R theta[2]: unknown  flows
    |}]

let%expect_test "ZIV: symbolic constants" =
  print_verdicts ~var:"v"
    {|
      data { int k; }
      parameters { real mu; }
      model {
        vector[k + 2] v;
        v[k + 1] = mu;
        v[k + 2] = 1;
        if (v[k + 2] > 0) target += 1;   // same symbol k, constants one apart
      }
    |};
  [%expect
    {|
    9: if((v[(k + 2)] > 0)) 10
      6 W v --> R v[k+2]: unknown  flows
      7 W v[k+1] --> R v[k+2]: independent
      8 W v[k+2] --> R v[k+2]: unknown  flows
    |}]

let%expect_test "Nested loops: the outer loop re-executes the body" =
  print_verdicts ~var:"b"
    {|
      data { int N; int M; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c;
        for (m in 1:M) {
          for (n in 1:(N - 1)) {
            c[n] = b[n + 1];   // carried by m: written in an earlier m iteration
            b[n] = a;
          }
        }
      }
    |};
  [%expect
    {|
    12: c[n] = b[(n + 1)];
      5 W b --> R b[n+1]: unknown  flows
      13 W b[n] --> R b[n+1]: constrained [m:*][n:-1]  flows
    |}]

let%expect_test "Nested loops: direction at the outer level" =
  print_verdicts ~var:"b"
    {|
      data { int N; int M; }
      parameters { real a; }
      model {
        vector[N] b; matrix[N, M] c; matrix[N, M] d;
        for (n in 2:(N - 1)) {
          for (m in 1:M) {
            c[n, m] = b[n + 1];   // written in a later n iteration: cannot flow
            d[n, m] = b[n - 1];   // written in an earlier n iteration: flows
            b[n] = a;
          }
        }
      }
    |};
  [%expect
    {|
    16: c[n, m] = b[(n + 1)];
      5 W b --> R b[n+1]: unknown  flows
      18 W b[n] --> R b[n+1]: constrained [n:-1][m:*]  cannot flow
    17: d[n, m] = b[(n - 1)];
      5 W b --> R b[n-1]: unknown  flows
      18 W b[n] --> R b[n-1]: constrained [n:1][m:*]  flows
    |}]

let%expect_test "Nested loops: a while loop between the for and the statements"
    =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; int k = 0;
        for (n in 1:N) {
          while (k < 2) {
            c[n] = b[n];   // the write below reaches it through the while loop
            b[n] = a;
            k += 1;
          }
        }
      }
    |};
  [%expect
    {|
    14: c[n] = b[n];
      5 W b --> R b[n]: unknown  flows
      15 W b[n] --> R b[n]: constrained [n:0][while:*]  flows
    |}]

let%expect_test "Indices the test cannot compare" =
  print_verdicts ~var:"b"
    {|
      data { int N; int j; int k; array[N] int<lower=1, upper=N> idx; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; matrix[N, N] d;
        b[k] = a;
        if (b[j] > 0) target += 1;        // different symbols
        for (n in 1:N) b[n] = a;
        for (n in 1:N) c[n] = b[n];       // sibling loops over the same name n
        for (n in 1:N) {
          for (m in 1:N) {
            b[n] = a;
            d[n, m] = b[m] + b[idx[n]];   // a different loop variable; a gather
          }
        }
        b[1:2] = c[1:2];                  // a slice
        if (b[k] > 0) target += 1;
      }
    |};
  [%expect
    {|
    12: if((b[j] > 0)) 13
      5 W b --> R b[j]: unknown  flows
      11 W b[k] --> R b[j]: unknown  flows
    19: c[n] = b[n];
      5 W b --> R b[n]: unknown  flows
      11 W b[k] --> R b[n]: unknown  flows
      16 W b[n] --> R b[n]: unknown  flows
    25: d[n, m] = (b[m] + b[idx[n]]);
      5 W b --> R b[m]: unknown  flows
      5 W b --> R b[?nonsequential]: unknown  flows
      11 W b[k] --> R b[m]: unknown  flows
      11 W b[k] --> R b[?nonsequential]: unknown  flows
      16 W b[n] --> R b[m]: unknown  flows
      16 W b[n] --> R b[?nonsequential]: unknown  flows
      24 W b[n] --> R b[m]: unknown  flows
      24 W b[n] --> R b[?nonsequential]: unknown  flows
    27: if((b[k] > 0)) 28
      5 W b --> R b[k]: unknown  flows
      11 W b[k] --> R b[k]: unknown  flows
      16 W b[n] --> R b[k]: unknown  flows
      24 W b[n] --> R b[k]: unknown  flows
      26 W b[1:2] --> R b[k]: unknown  flows
    |}]

let%expect_test "Indices that read a variable assigned in the block" =
  print_verdicts ~var:"v"
    {|
      data { int N; vector[N] x; }
      parameters { real mu; }
      model {
        vector[N] v; int m = 1;
        v[m] = mu;
        for (n in 1:N) {
          v[n] = v[m] + x[n];   // m changes below, so v[m] may be any element
          m = n;
        }
      }
    |};
  [%expect
    {|
    11: v[n] = (v[m] + x[n]);
      5 W v --> R v[?written]: unknown  flows
      8 W v[?written] --> R v[?written]: unknown  flows
      11 W v[n] --> R v[?written]: unknown  flows
    |}]

let%expect_test "A loop that walks down" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d;
        for (m in 1:(N - 1)) {
          int n = N - m;
          c[n] = b[n + 1];           // n is assigned in the loop
          b[n] = a;
        }
        for (m in 1:(N - 1)) {
          d[N - m] = b[N - m + 1];   // N - m is not the loop variable plus a constant
          b[N - m] = a;
        }
      }
    |};
  [%expect
    {|
    14: c[n] = b[(n + 1)];
      5 W b --> R b[?written]: unknown  flows
      15 W b[?written] --> R b[?written]: unknown  flows
    18: d[(N - m)] = b[((N - m) + 1)];
      5 W b --> R b[?nonsequential]: unknown  flows
      15 W b[?written] --> R b[?nonsequential]: unknown  flows
      19 W b[?nonsequential] --> R b[?nonsequential]: unknown  flows
    |}]

let%expect_test "Tuple fields" =
  print_verdicts ~var:"t"
    {|
      parameters { real a; }
      model {
        tuple(real, real) t;
        t.1 = a;
        t.2 = 1;
        if (t.2 > 0) target += 1;   // a different field
        if (t.1 > 0) target += 1;
      }
    |};
  [%expect
    {|
    7: if((t.2 > 0)) 8
      4 W t --> R t.2: unknown  flows
      5 W t.1 --> R t.2: independent
      6 W t.2 --> R t.2: unknown  flows
    9: if((t.1 > 0)) 10
      4 W t --> R t.1: unknown  flows
      5 W t.1 --> R t.1: unknown  flows
      6 W t.2 --> R t.1: independent
    |}]

let%expect_test "Paths of different length" =
  print_verdicts ~var:"a"
    {|
      data { int N; int K; }
      parameters { real mu; }
      model {
        array[N] vector[K] a; real y;
        for (n in 1:N) {
          a[n] = rep_vector(mu, K);
          y = a[n, 1];   // one index against two
        }
      }
    |};
  [%expect
    {|
    11: y = a[n, 1];
      6 W a --> R a[n, 1]: unknown  flows
      10 W a[n] --> R a[n, 1]: unknown  flows
    |}]

let%expect_test "Two index positions of one pair of accesses" =
  print_verdicts ~var:"m"
    {|
      data { int N; array[N] int<lower=1, upper=N> idx; }
      parameters { real a; }
      model {
        matrix[N, N] m; vector[N] d; vector[N] e; vector[N] f; vector[N] g;
        for (n in 2:N) {
          m[1, n] = a;
          d[n] = m[2, n];           // the first position rules the pair out
        }
        for (n in 2:N) {
          m[n, 1] = a;
          e[n] = m[n, idx[n]];      // the second position cannot be compared, so the first decides
        }
        for (n in 2:N) {
          m[n, n] = a;
          f[n] = m[n - 1, n];       // distances 1 and 0 at once: no iteration
        }
        for (n in 2:N) {
          m[n, n] = a;
          g[n] = m[n - 1, n - 1];   // both positions agree on distance 1
        }
      }
    |};
  [%expect
    {|
    18: d[n] = m[2, n];
      6 W m --> R m[2, n]: unknown  flows
      17 W m[1, n] --> R m[2, n]: independent
    22: e[n] = m[n, idx[n]];
      6 W m --> R m[n, ?nonsequential]: unknown  flows
      17 W m[1, n] --> R m[n, ?nonsequential]: unknown  flows
      21 W m[n, 1] --> R m[n, ?nonsequential]: constrained [n:0]  flows
    26: f[n] = m[(n - 1), n];
      6 W m --> R m[n-1, n]: unknown  flows
      17 W m[1, n] --> R m[n-1, n]: unknown  flows
      21 W m[n, 1] --> R m[n-1, n]: unknown  flows
      25 W m[n, n] --> R m[n-1, n]: independent
    30: g[n] = m[(n - 1), (n - 1)];
      6 W m --> R m[n-1, n-1]: unknown  flows
      17 W m[1, n] --> R m[n-1, n-1]: unknown  flows
      21 W m[n, 1] --> R m[n-1, n-1]: unknown  flows
      25 W m[n, n] --> R m[n-1, n-1]: unknown  flows
      29 W m[n, n] --> R m[n-1, n-1]: constrained [n:1]  flows
    |}]

let%expect_test "Several reads of one variable in one statement" =
  print_verdicts ~var:"x"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] x; vector[N] e;
        for (n in 3:N) {
          x[n] = a;
          e[n] = x[n - 1] + x[n - 2];   // one verdict per pair, no merge
        }
      }
    |};
  [%expect
    {|
    11: e[n] = (x[(n - 1)] + x[(n - 2)]);
      5 W x --> R x[n-1]: unknown  flows
      5 W x --> R x[n-2]: unknown  flows
      10 W x[n] --> R x[n-1]: constrained [n:1]  flows
      10 W x[n] --> R x[n-2]: constrained [n:2]  flows
    |}]
