open Std
open Std.Sexp_conv
open Analysis_and_optimization.Dependence_analysis
open Middle
open Analysis_and_optimization.Dataflow_types

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

let%expect_test "Reaching defns example" =
  let deps =
    LabelMap.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        ( reaching_defn_lookup x.reaching_defn_entry "j"
        , reaching_defn_lookup x.reaching_defn_exit "j" )) in
  print_s [%sexp (deps : (label Set.Poly.t * label Set.Poly.t) LabelMap.t)];
  [%expect
    {|
      ((1 (() ())) (2 ((9) (9))) (3 (() ())) (4 (() ())) (5 (() ())) (6 (() ()))
       (7 (() ())) (8 ((9) (9))) (9 ((9) (9))) (10 ((9) (9))) (11 ((9) (9)))
       (12 ((9) (9))) (13 ((9) (9))) (14 ((9) (9))) (15 ((9) (9))) (16 ((9) (9)))
       (17 ((9) (9))) (18 ((9) (9))) (19 ((9) (9))) (20 ((9) (9))) (21 ((9) (9)))
       (22 ((9) (9))))
    |}]

let%expect_test "Reaching defns example" =
  let deps =
    LabelMap.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) -> (x.reaching_defn_entry, x.reaching_defn_exit)) in
  print_s
    [%sexp
      (deps : (reaching_defn Set.Poly.t * reaching_defn Set.Poly.t) LabelMap.t)];
  [%expect
    {|
    ((1 (() ())) (2 (((i 4) (j 9)) ((i 4) (j 9)))) (3 (() ((i 3))))
     (4 (((i 3)) ((i 4)))) (5 (((i 4)) ((i 4)))) (6 (((i 4)) ((i 4))))
     (7 (((i 4)) ((i 4)))) (8 (((i 4) (j 9)) ((i 4) (j 9))))
     (9 (((i 4) (j 9)) ((i 4) (j 9)))) (10 (((i 4) (j 9)) ((i 4) (j 9))))
     (11 (((i 4) (j 9)) ((i 4) (j 9)))) (12 (((i 4) (j 9)) ((i 4) (j 9))))
     (13 (((i 4) (j 9)) ((i 4) (j 9)))) (14 (((i 4) (j 9)) ((i 4) (j 9))))
     (15 (((i 4) (j 9)) ((i 4) (j 9)))) (16 (((i 4) (j 9)) ((i 4) (j 9))))
     (17 (((i 4) (j 9)) ((i 4) (j 9)))) (18 (((i 4) (j 9)) ((i 4) (j 9))))
     (19 (((i 4) (j 9)) ((i 4) (j 9)))) (20 (((i 4) (j 9)) ((i 4) (j 9))))
     (21 (((i 4) (j 9)) ((i 4) (j 9)))) (22 (((i 4) (j 9)) ((i 4) (j 9)))))
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

(* ---- Access model: which elements each node reads and writes ---- *)

(** Prints [k+1], [k], [-2]; with [leading] the symbol drops the leading [+] and
    a bare constant is printed even when the constant is [0]. *)
let pp_linear ~leading ppf ({const; symbol} : linear) =
  let sign ~first value = if value < 0 then "-" else if first then "" else "+" in
  Option.iter symbol ~f:(fun symbol ->
      Fmt.pf ppf "%s%a" (sign ~first:leading 1) Expr.Typed.pp symbol);
  let bare = Option.is_none symbol in
  if const <> 0 || (leading && bare) then
    Fmt.pf ppf "%s%d" (sign ~first:(leading && bare) const) (abs const)

let pp_varying_kind ppf = function
  | Written -> Fmt.string ppf "written"
  | Nonlinear -> Fmt.string ppf "nonlinear"

(** [i], [i+1], [i+k-1] for [Affine]; [3], [k+1] for [Invariant]; [?gather],
    [?written], ... for [Varying]. *)
let pp_point ppf = function
  | Invariant offset -> pp_linear ~leading:true ppf offset
  | Affine offset ->
      Fmt.string ppf "i";
      pp_linear ~leading:false ppf offset
  | Varying kind -> Fmt.pf ppf "?%a" pp_varying_kind kind

(** Prints [i+1], [:], [k:], [1:k]; a multi-index is braced as [{idxs}] because
    [Index.pp] prints a multi-index like a single index. *)
let pp_subscript ppf (index : point Index.t) =
  match index with
  | MultiIndex indices -> Fmt.pf ppf "{%a}" pp_point indices
  | All | Single _ | Upfrom _ | Between _ -> Index.pp pp_point ppf index

(** [W v[i+1]], [R v], [+= target]. *)
let pp_access ppf {var; subs; kind; _} =
  Fmt.pf ppf "%s %s"
    (match kind with Write -> "W" | Read -> "R" | Increment -> "+=")
    var;
  if not (List.is_empty subs) then
    Fmt.pf ppf "[%a]" Fmt.(list ~sep:(any ", ") pp_subscript) subs

(** One line [label: accesses] per label that has accesses; labels without
    accesses (blocks, [break], ...) are left out. *)
let pp_node_accesses ppf (statement_map : dep_info_map) =
  LabelMap.iter statement_map ~f:(fun ~key ~data:(_, info) ->
      match info.accesses with
      | [] -> ()
      | accesses ->
          Fmt.pf ppf "%d: %a@." key
            Fmt.(list ~sep:(any ", ") pp_access)
            accesses)

let print_node_accesses prog =
  Fmt.pr "%a" pp_node_accesses
    (log_prob_build_dep_info_map (Test_utils.mir_of_string prog))

let%expect_test "Single indices: affine, invariant and varying" =
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
    3: R N
    4: W y
    5: W m
    6: W m
    7: R N, W n
    9: R v[i+1], R v[i-2], R v[i+1], R v[k], R k, R v[3], W y[i]
    10: R v[?nonlinear], R idx[i], R v[?nonlinear], R v[i+k], R k, R v[?written], R m, W y[i]
    11: R v[i+k-1], R k, R v[?nonlinear], R k, R v[?nonlinear], R k, R k, R v[i], R k, R k, R v[k+1], R k, R v[?nonlinear], R N, R v[?nonlinear], R k, R v[?nonlinear], W y[i]
    12: W m
    13: R J, W j
    15: R n, R v[i], W y[?written]
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
    3: R N
    4: W y
    5: R K
    6: W r
    7: R N
    8: W c
    9: R N, W n
    11: R v[:], R v[a:], R a, R v[a:b], R a, R b, R v[1:b], R b, R v[{idx}], R idx, W y[i]
    12: R v[i:], R v[i:i+1], R v[?nonlinear:N], R idx[i], R N, R v[{?nonlinear}], R pairs[i], W y[i]
    13: R m[i, :], W r
    14: R m[i, 1:K], R K, W r
    15: R m[:, 1], W c
    16: R m[{idx}, 2], R idx, W c
    17: R N, R y[1:N-1], R N, W c[2:N]
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
    2: W mu
    4: R N
    5: W v
    6: W acc
    7: W acc
    8: R N, W n
    10: W t
    11: R x[i], W t
    12: W u
    13: R t, R w[i], W v[i]
    14: R acc, R x[i], R w[i], W acc
    15: R x[i], R v[i], R mu, += target
    16: R v[i]
    17: R v[i]
    18: R v, W v[i]
    19: R v[i]
    20: W v[i]
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
    2: W mu
    4: R N
    5: W v
    6: W theta
    7: W m
    8: W m
    9: R mu, W theta[1]
    10: R x[k], R k, W theta[2]
    11: R m, R x[?written], R m, W v[?written]
    12: R N, W n
    14: R x[i+1], R theta[1], R v[?nonlinear], R idx[i], R v[?written], R m, W v[i]
    15: W j
    17: R v[?written], R n, R v[k], R k, R v[?written:N], R n, R N, W theta[i]
    18: R x, R v, R theta[2], += target
    |}]

let%expect_test "Right-hand-side variables of a set of labels" =
  let map = log_prob_build_dep_info_map accesses_example in
  print_s
    [%sexp
      (rhs_variables_at map (Set.Poly.of_list [10; 12]) : string Set.Poly.t)];
  [%expect {| (N k x) |}]

(* ---- Reaching definitions pruned by subscript ---- *)

(** For every node whose immediate dependencies are fewer than name-level
    reaching definitions would give, print the node, the definitions dropped
    because their subscripts cannot reach the node's reads, and the ones kept.
*)
let print_pruned_edges prog =
  let map = log_prob_build_dep_info_map (Test_utils.mir_of_string prog) in
  let name_level label =
    let stmt, info = LabelMap.find label map in
    let rhs =
      Analysis_and_optimization.Mir_utils.stmt_rhs_var_set stmt
      |> Set.Poly.map ~f:fst in
    Set.Poly.union info.parents
      (Set.Poly.union_map rhs
         ~f:(reaching_defn_lookup info.reaching_defn_entry)) in
  let pruned =
    LabelMap.fold map ~init:false ~f:(fun ~key:label ~data:_ pruned ->
        let plain = name_level label in
        let actual = node_immediate_dependencies map label in
        if Set.Poly.equal plain actual then pruned
        else (
          Fmt.pr "%d: dropped %a, kept %a@." label
            Fmt.(list ~sep:(any " ") int)
            (Set.Poly.to_list (Set.Poly.diff plain actual))
            Fmt.(list ~sep:(any " ") int)
            (Set.Poly.to_list actual);
          true)) in
  if not pruned then print_endline "no definition pruned"

let%expect_test "Pruning: distinct literal subscripts are independent" =
  print_pruned_edges
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
  [%expect {|
    8: dropped 7, kept 1 5 6
    9: dropped 6, kept 1 5 7
    |}]

let%expect_test "Pruning: same-iteration definitions inside a loop are kept" =
  print_pruned_edges
    {|
      data { int N; vector[N] x; vector[N] y; }
      parameters { real mu; real s; }
      model {
        vector[N] muj; vector[N] m;
        for (n in 1:N) {
          muj[n] = mu + x[n];
          m[n] = muj[n] * 2;
          y[n] ~ normal(m[n], s);
        }
      }
    |};
  [%expect {| no definition pruned |}]

let%expect_test "Pruning: whole-variable, gather and written symbols are kept" =
  print_pruned_edges
    {|
      data { int N; vector[N] x; array[N] int<lower=1, upper=N> idx; }
      parameters { real mu; }
      model {
        vector[N] v; real t; int k = 1;
        v[k + 1] = mu;
        k = 2;
        for (n in 1:N) {
          v[idx[n]] = mu + x[n];
          t = v[n] + v[k];
        }
        target += t + sum(v);
      }
    |};
  [%expect {| no definition pruned |}]

let uninitialized_var_example =
  Test_utils.mir_of_string
    {|
        functions {
          int f(int y) {
            int x;
            if (y > 2)
              return y + 24;
            return y + 2;
          }
        }
        data {
          real w;
        }
        transformed data {
          real wu;
          print(wu);
          print(w);
          wu = w;
        }
        parameters {
          real x;
        }
        model
        {
          int i;
          int z = 0;
          print(i);
          print(z);
          print(x);
          if (z == 1) {
            i = 1;
          } else {}
          print(i);
          if (z == 2) {
            i = 1;
          } else {
            i = 2;
          }
          print(i);
        }
        generated quantities {
          int k;
          print(k);
        }
      |}

let%expect_test "Uninitialized variables example" =
  let deps = mir_uninitialized_variables uninitialized_var_example in
  print_s [%sexp (deps : (Location_span.t * string) Set.Poly.t)];
  [%expect
    {|
      ((((begin_loc
          ((filename string) (line_num 15) (col_num 16) (byte_num 270)
           (included_from ())))
         (end_loc
          ((filename string) (line_num 15) (col_num 18) (byte_num 272)
           (included_from ()))))
        wu)
       (((begin_loc
          ((filename string) (line_num 26) (col_num 16) (byte_num 450)
           (included_from ())))
         (end_loc
          ((filename string) (line_num 26) (col_num 17) (byte_num 451)
           (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 32) (col_num 16) (byte_num 573)
           (included_from ())))
         (end_loc
          ((filename string) (line_num 32) (col_num 17) (byte_num 574)
           (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 42) (col_num 16) (byte_num 764)
           (included_from ())))
         (end_loc
          ((filename string) (line_num 42) (col_num 17) (byte_num 765)
           (included_from ()))))
        k))
    |}]
