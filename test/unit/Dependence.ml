(* These tests check {!Analysis_and_optimization.Dependence}. The module decides
   whether a write can supply the value that a read uses.
   {!Analysis_and_optimization.Dependence_analysis} uses the module to drop the
   writes that never supply the value.

   The tests build a map of statements with [log_prob_build_dep_info_map]. A
   label is the integer that identifies a statement in this map. The map covers
   the log density, which contains the statements of the parameters, transformed
   parameters and model blocks. An access is one read, write or increment of a
   variable ({!Analysis_and_optimization.Accesses}). The output prints a read as
   R and a write as W, then the variable and the indices. ? marks an index
   without a form ({!Analysis_and_optimization.Index_form}), such as an index
   that reads a variable that the log density writes, or a loop variable in a
   form other than a sum.

   A write reaches a read when the program can run the write before the read,
   and no write between them replaces the same element. Each write that reaches
   a read is a candidate source, and the read is the sink. Each test asks
   whether a candidate source can supply the value of the sink.

   A common loop is a loop around both the source and the sink. The distance at
   a common loop is the iteration of the sink minus the iteration of the source.
   In [b[n] = a; c[n] = b[n - 1];] the read of [b[n - 1]] uses the write to
   [b[n]] from one iteration earlier, so the distance is 1. A dependency with a
   nonzero distance at some loop is loop-carried. A dependency with distance 0
   at each loop is loop-independent.

   The test names refer to two standard dependence tests. The ZIV test (zero
   index variables) compares two indices without a loop variable, such as
   [theta[1]] and [theta[2]]. The strong SIV test (single index variable)
   compares two indices with the same loop variable, such as [b[n]] and [b[n -
   1]], and finds the distance.

   Each test prints the reads of one variable, with the label of each read.
   Under a read, each line compares the read with one candidate source. A line
   holds the label of the write, the access of the write, [-->], the access of
   the read, the result and a last word. The result is independent, unknown, or
   one distance for each common loop, outermost first. [distances [n:1]] means
   distance 1 at the loop over [n], and [[while:*]] stands for a [while] loop.
   Independent means the two accesses never select the same element. Unknown
   means the accesses can select the same element, but no distance is known. A *
   distance is unknown. The last word is [flows] or [cannot flow]. [flows] means
   the written value can reach the read. [cannot flow] means the read runs
   before the write: the first nonzero distance is negative, or every distance
   is zero and the read comes first in the program. An independent line has no
   last word.

   A definition from outside the log density, such as data, has no access
   record. The test does not print such a definition. Thus a read can show fewer
   lines than the writes that reach the read. *)

open Std
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Dataflow_utils
open Analysis_and_optimization.Dataflow_types
module Accesses = Analysis_and_optimization.Accesses
module Dependence = Analysis_and_optimization.Dependence

let print_verdicts ~var prog =
  let info = log_prob_build_dep_info_map (Test_utils.mir_of_string prog) in
  let accesses_of statement_id =
    Accesses.of_var var (LabelMap.find statement_id info.statements).accesses
  in
  let loops_of statement_id =
    let statement = LabelMap.find statement_id info.statements in
    loops_around ~statement_id statement.pattern statement.controls in
  let pp_access = Test_utils.one_line Accesses.pp_access in
  LabelMap.iter info.statements ~f:(fun ~key:sink_id ~data:sink_statement ->
      let sinks =
        List.filter (accesses_of sink_id) ~f:(fun sink ->
            match sink.Accesses.kind with
            | Accesses.Read | Increment -> true
            | Write -> false) in
      if not (List.is_empty sinks) then (
        Fmt.pr "%d: %a@." sink_id Test_utils.pp_stmt sink_statement.pattern;
        let reaching_ids =
          reaching_defn_lookup
            (LabelMap.find sink_id info.reaching_definitions)
            var in
        LabelMap.iter info.statements ~f:(fun ~key:source_id ~data:_ ->
            if Set.Poly.mem (Some source_id) reaching_ids then
              List.iter (accesses_of source_id) ~f:(fun source ->
                  List.iter sinks ~f:(fun sink ->
                      if flows_into source sink then
                        let source = (source_id, source) in
                        let sink = (sink_id, sink) in
                        let dependence = dependence_between info ~source ~sink in
                        let outcome =
                          match dependence with
                          | Dependence.Independent -> ""
                          | Distances _ ->
                              if may_supply info ~source ~sink then "  flows"
                              else "  cannot flow" in
                        let loops =
                          common_loops (loops_of source_id) (loops_of sink_id)
                        in
                        Fmt.pr "  %d %a --> %a: %a%s@." source_id pp_access
                          (snd source) pp_access (snd sink)
                          (Dependence.pp ~loops) dependence outcome)))))

(* [must_overwrite] decides whether a later write kills an earlier one. The test
   finds each write by the printed access. A write to [m[1]] overwrites [m[1,
   2]], because the row [m[1]] contains the element [m[1, 2]]. A write to [m[n,
   1]] overwrites itself only within one iteration of the loop over [n]. A slice
   write never overwrites, because a slice has no form. *)
let%expect_test "Comparing write paths" =
  let info =
    log_prob_build_dep_info_map
      (Test_utils.mir_of_string
         {|
      data { int N; int k; }
      parameters { real a; row_vector[N] r; }
      model {
        matrix[N, N] m;
        for (n in 2:N) {
          m[1] = r;
          m[1, 2] = a;
          m[k, 1] = a;
          m[n, 1] = a;
          m[n - 1, 1] = a;
          m[1:2, 1] = [a, a]';
        }
      }
    |})
  in
  let pp_access = Test_utils.one_line Accesses.pp_access in
  let path_of printed =
    LabelMap.fold info.statements ~init:None
      ~f:(fun ~key:_ ~data:statement found ->
        match found with
        | Some _ -> found
        | None ->
            List.find_map statement.accesses
              ~f:(fun (access : Accesses.access) ->
                if String.equal (Fmt.str "%a" pp_access access) printed then
                  Some access.path
                else None))
    |> Option.get in
  let must ~same_iteration killer killed =
    Fmt.pr "must_overwrite ~same_iteration:%b %s %s = %b@." same_iteration
      killer killed
      (Dependence.must_overwrite ~same_iteration ~killer:(path_of killer)
         ~killed:(path_of killed)) in
  must ~same_iteration:false "W m[1]" "W m[1, 2]";
  must ~same_iteration:false "W m[1, 2]" "W m[1]";
  must ~same_iteration:false "W m[k, 1]" "W m[k, 1]";
  must ~same_iteration:false "W m[n, 1]" "W m[n, 1]";
  must ~same_iteration:true "W m[n, 1]" "W m[n, 1]";
  must ~same_iteration:true "W m[n, 1]" "W m[n-1, 1]";
  must ~same_iteration:true "W m[1:2, 1]" "W m[1:2, 1]";
  [%expect
    {|
    must_overwrite ~same_iteration:false W m[1] W m[1, 2] = true
    must_overwrite ~same_iteration:false W m[1, 2] W m[1] = false
    must_overwrite ~same_iteration:false W m[k, 1] W m[k, 1] = true
    must_overwrite ~same_iteration:false W m[n, 1] W m[n, 1] = false
    must_overwrite ~same_iteration:true W m[n, 1] W m[n, 1] = true
    must_overwrite ~same_iteration:true W m[n, 1] W m[n-1, 1] = false
    must_overwrite ~same_iteration:true W m[1:2, 1] W m[1:2, 1] = false
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
      5 W x --> R x[n]: unknown  flows
      8 W x[n] --> R x[n-1]: distances [n:1]  flows
      8 W x[n] --> R x[n]: distances [n:0]  cannot flow
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
            d[n, m] = b[m] + b[idx[n]];   // m is another loop variable, and idx[n] has no form
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
      5 W b --> R b[?]: unknown  flows
      11 W b[k] --> R b[m]: unknown  flows
      11 W b[k] --> R b[?]: unknown  flows
      16 W b[n] --> R b[m]: unknown  flows
      16 W b[n] --> R b[?]: unknown  flows
      24 W b[n] --> R b[m]: unknown  flows
      24 W b[n] --> R b[?]: unknown  flows
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
      5 W v --> R v[?]: unknown  flows
      8 W v[?] --> R v[?]: unknown  flows
      11 W v[n] --> R v[?]: unknown  flows
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
      5 W b --> R b[?]: unknown  flows
      17 W b[?] --> R b[?]: unknown  flows
    20: d[(N - m)] = b[((N - m) + 1)];
      5 W b --> R b[-m+N+1]: unknown  flows
      17 W b[?] --> R b[-m+N+1]: unknown  flows
      21 W b[-m+N] --> R b[-m+N+1]: distances [m:1]  flows
    24: e[((N - m) + 1)] = b[(N - m)];
      5 W b --> R b[-m+N]: unknown  flows
      17 W b[?] --> R b[-m+N]: unknown  flows
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
          e[n] = m[n, idx[n]];      // the first index decides, because idx[n] has no form
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
      6 W m --> R m[n, ?]: unknown  flows
      17 W m[1, n] --> R m[n, ?]: unknown  flows
      21 W m[n, 1] --> R m[n, ?]: distances [n:0]  flows
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
      12 W v[n] --> R v[n]: distances [n:0][while:*]  flows
    12: v[n] = (v[n] + a);
      5 W v --> R v[n]: unknown  flows
      12 W v[n] --> R v[n]: distances [n:0][while:*]  flows
    15: for(i in 1:k[n]) 16
      8 W k --> R k[n]: unknown  flows
      17 W k[n] --> R k[n]: distances [n:0][i:*]  flows
    17: k[n] = (k[n] - 1);
      8 W k --> R k[n]: unknown  flows
      17 W k[n] --> R k[n]: distances [n:0][i:*]  flows
    |}]
