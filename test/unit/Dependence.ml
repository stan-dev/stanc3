open Std
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_utils
open Analysis_and_optimization.Dataflow_types
module Dependence = Analysis_and_optimization.Dependence
module Accesses = Dependence.Accesses
module Access_function = Dependence.Access_function

(* Prints, under each read of [var], one line per reaching write of [var]: {v 12
   W b[n] --> R b[n-1]: distances [n:1] flows ^writer ^write ^read
   ^Dependence.pp ^can the write run first v} The last word is [flows] or
   [cannot flow], and an [independent] line has no last word. A [*] distance is
   unknown. *)
let print_verdicts ~var prog =
  let statements =
    build_log_prob_dependence_info (Test_utils.mir_of_string prog) in
  let accesses_of (statement : stmt_info) =
    List.filter statement.accesses ~f:(fun (access : access) ->
        String.equal access.var var) in
  (* The kinds that can form a flow dependence: the write side writes and the
     read side reads. *)
  let is_flow_kind (write : access) (read : access) =
    (match write.kind with Write | ReadWrite -> true | Read -> false)
    && match read.kind with Read | ReadWrite -> true | Write -> false in
  let loop_name = Test_utils.loop_name statements in
  let pp_access =
    Test_utils.one_line (Accesses.pp_access (Access_function.pp ~loop_name))
  in
  LabelMap.iter statements ~f:(fun ~key:reader_id ~data:reader ->
      let reads =
        List.filter (accesses_of reader) ~f:(fun read ->
            match read.Accesses.kind with
            | Accesses.Read | ReadWrite -> true
            | Write -> false) in
      if not (List.is_empty reads) then (
        Fmt.pr "%d: %a@." reader_id Test_utils.pp_stmt reader.pattern;
        let reaching_ids =
          Set.Poly.filter_map reader.reaching_definitions ~f:(fun definition ->
              if String.equal definition.access.var var then
                definition.statement_id
              else None) in
        LabelMap.iter statements ~f:(fun ~key:writer_id ~data:writer ->
            if Set.Poly.mem writer_id reaching_ids then
              List.iter (accesses_of writer) ~f:(fun write ->
                  List.iter reads ~f:(fun read ->
                      if is_flow_kind write read then
                        let dependence =
                          Dependence.test_dependence
                            (writer.loop_ids, write.path)
                            (reader.loop_ids, read.path) in
                        let outcome =
                          if not (Dependence.is_overlap_possible dependence)
                          then ""
                          else if
                            Dependence.is_writer_possibly_first
                              ~is_writer_first:(writer_id < reader_id)
                              dependence
                          then "  flows"
                          else "  cannot flow" in
                        Fmt.pr "  %d %a --> %a: %a%s@." writer_id pp_access
                          write pp_access read (Dependence.pp ~loop_name)
                          dependence outcome)))))

(* [is_overwritten] decides whether a later write kills an earlier one. A write
   to [m[1]] overwrites [m[1, 2]], because the row [m[1]] contains the element
   [m[1, 2]]. A write to [m[n, 1]] overwrites itself only within one iteration
   of the loop over [n]. The comparison does not analyze slices, so a slice
   write kills nothing. *)
let%expect_test "Comparing write paths" =
  let statements =
    build_log_prob_dependence_info
      (Test_utils.mir_of_string
         {|
        data {
          int N;
          int k;
        }
        parameters {
          matrix[N, N] m;
        }
        model {
          matrix[N, N] w;
          int j = k;
          for (n in 2:N) {
            w[1] = m[1];
            w[1, 2] = m[1, 2];
            w[k, 1] = m[k, 1];
            w[n, 1] = m[n, 1];
            w[n - 1, 1] = m[n - 1, 1];
            w[1:2, 1] = m[1:2, 1];
            w[k + k, 1] = m[1, 1];
            w[2 * k, 1] = m[1, 1];
            w[n + n, 1] = m[1, 1];
            w[2 * n, 1] = m[1, 1];
            w[j + j, 1] = m[1, 1];
            w[2 * j, 1] = m[1, 1];
            j = j + 1;
          }
        }
      |})
  in
  let loop_name = Test_utils.loop_name statements in
  let pp_path =
    Test_utils.one_line (Accesses.pp_path (Access_function.pp ~loop_name)) in
  (* The paths of the writes to [w], without the declaration, in source
     order. *)
  let write_paths =
    List.concat_map (LabelMap.bindings statements) ~f:(fun (_, statement) ->
        List.filter_map statement.accesses ~f:(fun (access : access) ->
            match (access.var, access.kind, access.path) with
            | "w", Write, _ :: _ -> Some access.path
            | _ -> None)) in
  let print_overwrite ~is_same_iteration killer killed =
    Fmt.pr "w%a overwrites w%a%s: %b@." pp_path
      (List.nth write_paths killer)
      pp_path
      (List.nth write_paths killed)
      (if is_same_iteration then " in the same iteration" else "")
      (Dependence.is_overwritten ~is_same_iteration
         (List.nth write_paths killer)
         (List.nth write_paths killed)) in
  print_overwrite ~is_same_iteration:false 0 1;
  print_overwrite ~is_same_iteration:false 1 0;
  print_overwrite ~is_same_iteration:false 2 2;
  print_overwrite ~is_same_iteration:false 3 3;
  print_overwrite ~is_same_iteration:true 3 3;
  print_overwrite ~is_same_iteration:true 3 4;
  print_overwrite ~is_same_iteration:true 5 5;
  print_overwrite ~is_same_iteration:false 7 6;
  print_overwrite ~is_same_iteration:false 9 8;
  print_overwrite ~is_same_iteration:true 9 8;
  print_overwrite ~is_same_iteration:true 11 10;
  [%expect
    {|
    w[1] overwrites w[1, 2]: true
    w[1, 2] overwrites w[1]: false
    w[k, 1] overwrites w[k, 1]: true
    w[n, 1] overwrites w[n, 1]: false
    w[n, 1] overwrites w[n, 1] in the same iteration: true
    w[n, 1] overwrites w[n-1, 1] in the same iteration: false
    w[1:2, 1] overwrites w[1:2, 1] in the same iteration: false
    w[2*k, 1] overwrites w[2*k, 1]: true
    w[2*n, 1] overwrites w[2*n, 1]: false
    w[2*n, 1] overwrites w[2*n, 1] in the same iteration: true
    w[(2 * j), 1] overwrites w[(j + j), 1] in the same iteration: false
    |}]

let%expect_test "Strong SIV: loop-carried and loop-independent" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d;
        for (n in 2:N) {
          b[n] = a;
          c[n] = b[n - 1];   // the read gets the write from the previous iteration
          d[n] = b[n];       // the write runs first in the same iteration
        }
      }
    |};
  [%expect
    {|
    13: c[n] = b[(n - 1)];
      5 W b --> R b[n-1]: unknown  flows
      12 W b[n] --> R b[n-1]: distances [n:1]  flows
    14: d[n] = b[n];
      5 W b --> R b[n]: unknown  flows
      12 W b[n] --> R b[n]: distances [n:0]  flows
    |}]

let%expect_test "Strong SIV: self pairs" =
  print_verdicts ~var:"x"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] x;
        for (n in 2:N) {
          x[n] = x[n - 1] + x[n] + a;   // x[n] cannot flow because the write runs after the read
        }
      }
    |};
  [%expect
    {|
    8: x[n] = ((x[(n - 1)] + x[n]) + a);
      5 W x --> R x[n-1]: unknown  flows
      5 W x --> RW x[n]: unknown  flows
      8 RW x[n] --> R x[n-1]: distances [n:1]  flows
      8 RW x[n] --> RW x[n]: distances [n:0]  cannot flow
    |}]

let%expect_test "Sink runs first" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d;
        for (n in 1:(N - 1)) {
          c[n] = b[n + 1];   // b[n] writes this element in the next iteration
          d[n] = b[n];       // b[n] writes this element later in this iteration
          b[n] = a;
        }
      }
    |};
  [%expect
    {|
    12: c[n] = b[(n + 1)];
      5 W b --> R b[n+1]: unknown  flows
      14 W b[n] --> R b[n+1]: distances [n:-1]  cannot flow
    13: d[n] = b[n];
      5 W b --> R b[n]: unknown  flows
      14 W b[n] --> R b[n]: distances [n:0]  cannot flow
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
        y ~ normal(theta[1], s);   // theta[2] = b never writes theta[1]
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
        if (v[k + 2] > 0) target += 1;   // the indices k + 1 and k + 2 always differ
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
            c[n] = b[n + 1];   // an earlier m iteration writes this element
            b[n] = a;
          }
        }
      }
    |};
  [%expect
    {|
    12: c[n] = b[(n + 1)];
      5 W b --> R b[n+1]: unknown  flows
      13 W b[n] --> R b[n+1]: distances [m:*][n:-1]  flows
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
            c[n, m] = b[n + 1];   // a later n iteration writes this element
            d[n, m] = b[n - 1];   // an earlier n iteration writes this element
            b[n] = a;
          }
        }
      }
    |};
  [%expect
    {|
    16: c[n, m] = b[(n + 1)];
      5 W b --> R b[n+1]: unknown  flows
      18 W b[n] --> R b[n+1]: distances [n:-1][m:*]  cannot flow
    17: d[n, m] = b[(n - 1)];
      5 W b --> R b[n-1]: unknown  flows
      18 W b[n] --> R b[n-1]: distances [n:1][m:*]  flows
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
            c[n] = b[n];   // the next while iteration reads the write below
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
      15 W b[n] --> R b[n]: distances [n:0][while:*]  flows
    |}]

let%expect_test "Indices the test cannot compare" =
  print_verdicts ~var:"b"
    {|
      data { int N; int j; int k; array[N] int<lower=1, upper=N> idx; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; matrix[N, N] d;
        b[k] = a;
        if (b[j] > 0) target += 1;        // j and k can be equal
        for (n in 1:N) b[n] = a;
        for (n in 1:N) c[n] = b[n];       // the n of another loop is a different variable
        for (n in 1:N) {
          for (m in 1:N) {
            b[n] = a;
            d[n, m] = b[m] + b[idx[n]];   // m is another loop variable, and idx[n] has no view
          }
        }
        b[1:2] = c[1:2];                  // the analysis does not compare slices
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
      5 W b --> R b[idx[n]]: unknown  flows
      11 W b[k] --> R b[m]: unknown  flows
      11 W b[k] --> R b[idx[n]]: unknown  flows
      16 W b[n] --> R b[m]: unknown  flows
      16 W b[n] --> R b[idx[n]]: unknown  flows
      24 W b[n] --> R b[m]: unknown  flows
      24 W b[n] --> R b[idx[n]]: unknown  flows
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
          v[n] = v[m] + x[n];   // the loop writes m, so v[m] can be any element
          m = n;
        }
      }
    |};
  [%expect
    {|
    11: v[n] = (v[m] + x[n]);
      5 W v --> R v[m]: unknown  flows
      8 W v[m] --> R v[m]: unknown  flows
      11 W v[n] --> R v[m]: unknown  flows
    |}]

let%expect_test "A loop that walks down" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d; vector[N] e;
        for (m in 1:(N - 1)) {
          int n = N - m;
          c[n] = b[n + 1];           // n is assigned in the loop
          b[n] = a;
        }
        for (m in 1:(N - 1)) {
          d[N - m] = b[N - m + 1];   // the previous iteration writes b[N - m + 1]
          b[N - m] = a;
        }
        for (m in 2:N) {
          e[N - m + 1] = b[N - m];   // the next iteration writes b[N - m]
          b[N - m + 1] = a;
        }
      }
    |};
  [%expect
    {|
    16: c[n] = b[(n + 1)];
      5 W b --> R b[(n + 1)]: unknown  flows
      17 W b[n] --> R b[(n + 1)]: unknown  flows
    20: d[(N - m)] = b[((N - m) + 1)];
      5 W b --> R b[-m+N+1]: unknown  flows
      17 W b[n] --> R b[-m+N+1]: unknown  flows
      21 W b[-m+N] --> R b[-m+N+1]: distances [m:1]  flows
    24: e[((N - m) + 1)] = b[(N - m)];
      5 W b --> R b[-m+N]: unknown  flows
      17 W b[n] --> R b[-m+N]: unknown  flows
      21 W b[-m+N] --> R b[-m+N]: unknown  flows
      25 W b[-m+N+1] --> R b[-m+N]: distances [m:-1]  cannot flow
    |}]

let%expect_test "Tuple fields" =
  print_verdicts ~var:"t"
    {|
      parameters { real a; }
      model {
        tuple(real, real) t;
        t.1 = a;
        t.2 = 1;
        if (t.2 > 0) target += 1;   // t.1 = a writes a different field
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
          y = a[n, 1];
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
          d[n] = m[2, n];           // row 1 and row 2 never match
        }
        for (n in 2:N) {
          m[n, 1] = a;
          e[n] = m[n, idx[n]];      // the first index decides, because idx[n] has no view
        }
        for (n in 2:N) {
          m[n, n] = a;
          f[n] = m[n - 1, n];       // no iteration gives distance 1 and distance 0
        }
        for (n in 2:N) {
          m[n, n] = a;
          g[n] = m[n - 1, n - 1];   // both indices give distance 1
        }
      }
    |};
  [%expect
    {|
    18: d[n] = m[2, n];
      6 W m --> R m[2, n]: unknown  flows
      17 W m[1, n] --> R m[2, n]: independent
    22: e[n] = m[n, idx[n]];
      6 W m --> R m[n, idx[n]]: unknown  flows
      17 W m[1, n] --> R m[n, idx[n]]: independent
      21 W m[n, 1] --> R m[n, idx[n]]: distances [n:0]  flows
    26: f[n] = m[(n - 1), n];
      6 W m --> R m[n-1, n]: unknown  flows
      17 W m[1, n] --> R m[n-1, n]: unknown  flows
      21 W m[n, 1] --> R m[n-1, n]: independent
      25 W m[n, n] --> R m[n-1, n]: independent
    30: g[n] = m[(n - 1), (n - 1)];
      6 W m --> R m[n-1, n-1]: unknown  flows
      17 W m[1, n] --> R m[n-1, n-1]: unknown  flows
      21 W m[n, 1] --> R m[n-1, n-1]: unknown  flows
      25 W m[n, n] --> R m[n-1, n-1]: unknown  flows
      29 W m[n, n] --> R m[n-1, n-1]: distances [n:1]  flows
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
          e[n] = x[n - 1] + x[n - 2];
        }
      }
    |};
  [%expect
    {|
    11: e[n] = (x[(n - 1)] + x[(n - 2)]);
      5 W x --> R x[n-1]: unknown  flows
      5 W x --> R x[n-2]: unknown  flows
      10 W x[n] --> R x[n-1]: distances [n:1]  flows
      10 W x[n] --> R x[n-2]: distances [n:2]  flows
    |}]

let%expect_test "Loop headers run in every iteration of their own loop" =
  let prog =
    {|
      data { int N; array[N] int<lower=0> K; }
      parameters { real a; }
      model {
        vector[N] v; array[N] int k = K;
        for (n in 1:N) {
          while (v[n] < 0) v[n] = v[n] + a;   // the write reaches the next condition check
        }
        for (n in 1:N) {
          for (i in 1:k[n]) k[n] = k[n] - 1;  // the write reaches the next bound check
        }
      }
    |}
  in
  print_verdicts ~var:"v" prog;
  print_verdicts ~var:"k" prog;
  [%expect
    {|
    11: while((v[n] < 0)) 12
      5 W v --> R v[n]: unknown  flows
      12 RW v[n] --> R v[n]: distances [n:0][while:*]  flows
    12: v[n] = (v[n] + a);
      5 W v --> RW v[n]: unknown  flows
      12 RW v[n] --> RW v[n]: distances [n:0][while:*]  flows
    15: for(i in 1:k[n]) 16
      8 W k --> R k[n]: unknown  flows
      17 RW k[n] --> R k[n]: distances [n:0][i:*]  flows
    17: k[n] = (k[n] - 1);
      8 W k --> RW k[n]: unknown  flows
      17 RW k[n] --> RW k[n]: distances [n:0][i:*]  flows
    |}]

(* A loop is identified by the label of the loop statement, not by the name of
   the loop variable. The two sibling loops both use [n], and no loop is around
   both the write and the read. The write of [b[n]] and the read of [b[n - 1]]
   thus have no distance, and the write of the first loop can supply any element
   that the second loop reads. *)
let%expect_test "Loop identity: the same variable name in sibling loops" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c;
        for (n in 1:N) b[n] = a;
        for (n in 2:N) c[n] = b[n - 1];
      }
    |};
  [%expect
    {|
    13: c[n] = b[(n - 1)];
      5 W b --> R b[n-1]: unknown  flows
      10 W b[n] --> R b[n-1]: unknown  flows
    |}]

let%expect_test "Normalized sums: equal values written differently" =
  print_verdicts ~var:"b"
    {|
      data { int N; int k; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c; vector[N] d;
        for (n in 2:N) {
          b[k + k] = a;
          c[n] = b[2 * k] + b[2 * k + 1];
          b[n + n - n] = a;
          d[n] = b[1 * n - 1] + b[-(1 - n)];
        }
      }
    |};
  [%expect
    {|
    13: c[n] = (b[(2 * k)] + b[((2 * k) + 1)]);
      5 W b --> R b[2*k]: unknown  flows
      5 W b --> R b[2*k+1]: unknown  flows
      12 W b[2*k] --> R b[2*k]: unknown  flows
      12 W b[2*k] --> R b[2*k+1]: independent
      14 W b[n] --> R b[2*k]: unknown  flows
      14 W b[n] --> R b[2*k+1]: unknown  flows
    15: d[n] = (b[((1 * n) - 1)] + b[-((1 - n))]);
      5 W b --> R b[n-1]: unknown  flows
      5 W b --> R b[n-1]: unknown  flows
      12 W b[2*k] --> R b[n-1]: unknown  flows
      12 W b[2*k] --> R b[n-1]: unknown  flows
      14 W b[n] --> R b[n-1]: distances [n:1]  flows
      14 W b[n] --> R b[n-1]: distances [n:1]  flows
    |}]

let%expect_test "Strong SIV: equal strides other than one" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c;
        for (n in 2:N) {
          b[2 * n] = a;
          c[n] = b[2 * n - 1] + b[2 * n - 2] + b[2 * n + 4];
          b[N - 3 * n] = a;
          c[n] = b[N - 3 * n + 6] + b[N - 3 * n + 1];
        }
      }
    |};
  [%expect
    {|
    11: c[n] = ((b[((2 * n) - 1)] + b[((2 * n) - 2)]) + b[((2 * n) + 4)]);
      5 W b --> R b[2*n-1]: unknown  flows
      5 W b --> R b[2*n-2]: unknown  flows
      5 W b --> R b[2*n+4]: unknown  flows
      10 W b[2*n] --> R b[2*n-1]: independent
      10 W b[2*n] --> R b[2*n-2]: distances [n:1]  flows
      10 W b[2*n] --> R b[2*n+4]: distances [n:-2]  cannot flow
      12 W b[-3*n+N] --> R b[2*n-1]: unknown  flows
      12 W b[-3*n+N] --> R b[2*n-2]: unknown  flows
      12 W b[-3*n+N] --> R b[2*n+4]: unknown  flows
    13: c[n] = (b[((N - (3 * n)) + 6)] + b[((N - (3 * n)) + 1)]);
      5 W b --> R b[-3*n+N+6]: unknown  flows
      5 W b --> R b[-3*n+N+1]: unknown  flows
      10 W b[2*n] --> R b[-3*n+N+6]: unknown  flows
      10 W b[2*n] --> R b[-3*n+N+1]: unknown  flows
      12 W b[-3*n+N] --> R b[-3*n+N+6]: distances [n:2]  flows
      12 W b[-3*n+N] --> R b[-3*n+N+1]: independent
    |}]

let%expect_test "GCD: strides with a common divisor" =
  print_verdicts ~var:"b"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] b; vector[N] c;
        for (n in 1:N) {
          b[2 * n] = a;
          c[n] = b[4 * n + 1] + b[4 * n + 2] + b[3 * n + 1];
          for (m in 1:N) {
            b[6 * n + 4 * m + 1] = a;
            c[m] = b[2 * m + 4 * n] + b[2 * m + 4 * n + 1];
          }
        }
      }
    |};
  [%expect
    {|
    11: c[n] = ((b[((4 * n) + 1)] + b[((4 * n) + 2)]) + b[((3 * n) + 1)]);
      5 W b --> R b[4*n+1]: unknown  flows
      5 W b --> R b[4*n+2]: unknown  flows
      5 W b --> R b[3*n+1]: unknown  flows
      10 W b[2*n] --> R b[4*n+1]: independent
      10 W b[2*n] --> R b[4*n+2]: unknown  flows
      10 W b[2*n] --> R b[3*n+1]: unknown  flows
      14 W b[6*n+4*m+1] --> R b[4*n+1]: unknown  flows
      14 W b[6*n+4*m+1] --> R b[4*n+2]: independent
      14 W b[6*n+4*m+1] --> R b[3*n+1]: unknown  flows
    15: c[m] = (b[((2 * m) + (4 * n))] + b[(((2 * m) + (4 * n)) + 1)]);
      5 W b --> R b[4*n+2*m]: unknown  flows
      5 W b --> R b[4*n+2*m+1]: unknown  flows
      10 W b[2*n] --> R b[4*n+2*m]: unknown  flows
      10 W b[2*n] --> R b[4*n+2*m+1]: independent
      14 W b[6*n+4*m+1] --> R b[4*n+2*m]: independent
      14 W b[6*n+4*m+1] --> R b[4*n+2*m+1]: unknown  flows
    |}]

let%expect_test "Literal bounds: a distance longer than the loop" =
  print_verdicts ~var:"v"
    {|
      parameters { real a; }
      model {
        vector[10] v = rep_vector(0, 10);
        for (n in 1:5) {
          v[n + 5] = a;
          if (v[n] > 0) target += 1;
          if (v[n + 1] > 0) target += 1;
        }
      }
    |};
  [%expect
    {|
    9: if((v[n] > 0)) 10
      5 W v --> R v[n]: unknown  flows
      8 W v[n+5] --> R v[n]: independent
    11: if((v[(n + 1)] > 0)) 12
      5 W v --> R v[n+1]: unknown  flows
      8 W v[n+5] --> R v[n+1]: distances [n:4]  flows
    |}]

let%expect_test "Weak-zero SIV: one fixed element against a loop" =
  print_verdicts ~var:"x"
    {|
      data { int N; }
      parameters { real a; }
      model {
        vector[N] x;
        x[1] = a;
        for (n in 2:N) {
          x[n] = x[1] + a;
        }
        for (n in 1:5) {
          if (x[5] > 0) target += 1;
          x[n] = a;
          if (x[1] > 0) target += 1;
        }
      }
    |};
  [%expect
    {|
    9: x[n] = (x[1] + a);
      5 W x --> R x[1]: unknown  flows
      6 W x[1] --> R x[1]: unknown  flows
      9 W x[n] --> R x[1]: independent
    12: if((x[5] > 0)) 13
      5 W x --> R x[5]: unknown  flows
      6 W x[1] --> R x[5]: independent
      9 W x[n] --> R x[5]: unknown  flows
      14 W x[n] --> R x[5]: distances [n:=,>]  cannot flow
    15: if((x[1] > 0)) 16
      5 W x --> R x[1]: unknown  flows
      6 W x[1] --> R x[1]: unknown  flows
      9 W x[n] --> R x[1]: independent
      14 W x[n] --> R x[1]: distances [n:<,=]  flows
    |}]
