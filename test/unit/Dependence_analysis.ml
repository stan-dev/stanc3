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
        ( reaching_defn_lookup x.reaching_defn_entry (VVar "j")
        , reaching_defn_lookup x.reaching_defn_exit (VVar "j") )) in
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
      ((1 (() ())) (2 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (3 (() (((VVar i) 3)))) (4 ((((VVar i) 3)) (((VVar i) 4))))
       (5 ((((VVar i) 4)) (((VVar i) 4)))) (6 ((((VVar i) 4)) (((VVar i) 4))))
       (7 ((((VVar i) 4)) (((VVar i) 4))))
       (8 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (9 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (10 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (11 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (12 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (13 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (14 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (15 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (16 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (17 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (18 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (19 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (20 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (21 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9))))
       (22 ((((VVar i) 4) ((VVar j) 9)) (((VVar i) 4) ((VVar j) 9)))))
    |}]

let%expect_test "Variable dependency example" =
  let deps =
    node_vars_dependencies
      (log_prob_build_dep_info_map example1_program)
      (Set.Poly.singleton (VVar "j"))
      17 in
  print_s [%sexp (deps : label Set.Poly.t)];
  [%expect {|
      (4 5 9 11 13 14 16)
    |}]

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
  let written = Stmt.Helpers.assigned_or_declared_variables body in
  Fmt.pr "loopvar: %s@.written: %a@." loopvar
    Fmt.(list ~sep:(any ", ") string)
    (Set.Poly.to_list written);
  let leaves = match body.pattern with Block l | SList l -> l | _ -> [body] in
  List.iteri leaves ~f:(fun i (s : Stmt.Located.t) ->
      Fmt.pr "%d: %s@.   %s@." i
        (one_line Stmt.Located.pp s)
        (one_line
           Fmt.(list ~sep:(any ", ") pp_access)
           (stmt_accesses ~loopvar ~written ~label:i s.pattern)))

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
        for (n in 1:N) {
          real t = 2 * x[n];
          vector[2] u;
          v[n] = t + w[n];
          target += normal_lpdf(x[n] | v[n], mu);
          if (v[n] > 0) print(v[n]); else v[n] = sum(v);
          while (v[n] < 0) v[n] = 0;
        }
      }
    |};
  [%expect
    {|
    loopvar: n
    written: t, u, v
    0: real t;
       W t
    1: t = (promote(2, real, data) * x[n]);
       R x[i], W t
    2: vector[2] u;
       W u
    3: v[n] = (t + w[n]);
       R t, R w[i], W v[i]
    4: target += normal_lpdf(x[n], v[n], mu);
       R x[i], R v[i], R mu
    5: if((v[n] > 0)) FnPrint__(v[n]); else v[n] = sum(v);
       R v[i], R v[i], R v, W v[i]
    6: while((v[n] < 0)) v[n] = promote(0, real, var);
       R v[i], W v[i]
    |}]

(* Direct tests of the dependence test, one per row of the §7.4 table. *)
let aff ?(coeff = 1) ?(terms = []) const = Affine {coeff; offset= {const; terms}}
let inv ?(terms = []) const = Invariant {const; terms}
let k = [(1, Expr.Helpers.variable "k")]
let m = [(1, Expr.Helpers.variable "m")]
let w subs = {var= "v"; subs; is_write= true; label= 0}
let r subs = {var= "v"; subs; is_write= false; label= 1}

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
    W v[i]  /  R v[i]  ->  dependent {=} distance 0
    W v[i+1]  /  R v[i]  ->  dependent {<} distance 1
    W v[i]  /  R v[i+1]  ->  dependent {>} distance -1
    W v[i+8]  /  R v[i]  ->  dependent {<} distance 8
    W v[-i]  /  R v[-i+2]  ->  dependent {<} distance 2
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
    W v[2i]  /  R v[i]  ->  dependent {<,=,>}
    W v[i]  /  R v[1]  ->  dependent {<,=,>}
    W v[1]  /  R v[2]  ->  independent
    W v[1]  /  R v[1]  ->  dependent {<,=,>}
    W v[k]  /  R v[1]  ->  dependent {<,=,>}
    W v[?gather]  /  R v[i]  ->  dependent {<,=,>}
    W v[i]  /  R v[i, 1]  ->  dependent {<,=,>}
    W v  /  R v  ->  dependent {<,=,>}
    W v  /  R v[i]  ->  dependent {<,=,>}
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
    W v[i+k]  /  R v[i+k]  ->  dependent {=} distance 0
    W v[i+k+1]  /  R v[i+k]  ->  dependent {<} distance 1
    W v[i+k]  /  R v[i]  ->  dependent {<,=,>}
    W v[i+k]  /  R v[i+m]  ->  dependent {<,=,>}
    W v[k+1]  /  R v[k+2]  ->  independent
    W v[k]  /  R v[k]  ->  dependent {<,=,>}
    W v[k]  /  R v[m]  ->  dependent {<,=,>}
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
    W v[i, 1]  /  R v[2, i]  ->  dependent {<,=,>}
    W v[i+1, i]  /  R v[i, ?slice]  ->  dependent {<} distance 1
    W v[i, 1]  /  R v[i, 1]  ->  dependent {=} distance 0
    W v[i+1, i+1]  /  R v[i, i]  ->  dependent {<} distance 1
    |}]

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
          ((filename string) (line_num 15) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 15) (col_num 18) (included_from ()))))
        wu)
       (((begin_loc
          ((filename string) (line_num 26) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 26) (col_num 17) (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 32) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 32) (col_num 17) (included_from ()))))
        i)
       (((begin_loc
          ((filename string) (line_num 42) (col_num 16) (included_from ())))
         (end_loc
          ((filename string) (line_num 42) (col_num 17) (included_from ()))))
        k))
    |}]
