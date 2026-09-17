open Analysis_and_optimization.Factor_graph
open Std
open Std.Sexp_conv
open Analysis_and_optimization.Dataflow_types

(** Both maps as association lists, the same shape the derived sexp had. *)
let print_factor_graph (graph : factor_graph) =
  print_s
    [%sexp
      { factor_map=
          (FactorMap.to_list graph.factor_map
            : ((factor * label) * string Set.Poly.t) list)
      ; var_map=
          (String.Map.to_list graph.var_map
            : (string * (factor * label) Set.Poly.t) list) }]

let reject_example =
  Test_utils.mir_of_string
    {|
        parameters {
          real x;
        }
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
                reject(0);               // 19
              } else
              {                          // 20
                target += 1 + 1;         // 21
              }
              print("Fin");              // 22
            }
          }
        }
      |}

let%expect_test "Factor graph reject example" =
  let deps = prog_factor_graph reject_example in
  print_factor_graph deps;
  [%expect
    {|
      ((factor_map
        (((Reject 20) ())
         (((TargetTerm
            ((pattern (Lit Int 1))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
           22)
          ())
         (((TargetTerm
            ((pattern (Lit Int 1))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
           22)
          ())))
       (var_map ()))
    |}]

let complex_example =
  Test_utils.mir_of_string
    {|
        parameters {
          real a;
          real b;
          real c;
          real d;
          real e;
          real f;
        }
        model
        {
          b ~ normal(0, 1);
          a ~ normal(b, 1);
          real x;
          if(a<0) {
            x = 1;
          } else {
            x = 2;
          }
          c ~ normal (x, 1);
          d ~ normal (c, b);
          real z = a + b + c + d;
          target += z * e + f*f;
        }
      |}

let%expect_test "Factor graph complex example" =
  let deps = prog_factor_graph complex_example in
  print_factor_graph deps;
  [%expect
    {|
    ((factor_map
      ((((TargetTerm
          ((pattern
            (FunApp (StanLib Times__ FnPlain AoS)
             (((pattern (Var f))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern (Var f))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
           (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
         21)
        (f))
       (((TargetTerm
          ((pattern
            (FunApp (StanLib Times__ FnPlain AoS)
             (((pattern (Var z))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern (Var e))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
           (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
         21)
        (a b c d e))
       (((TargetTerm
          ((pattern
            (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
             (((pattern (Var a))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern (Var b))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern
                (Promotion
                 ((pattern (Lit Int 1))
                  (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                 UReal DataOnly))
               (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
           (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
         10)
        (a b))
       (((TargetTerm
          ((pattern
            (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
             (((pattern (Var b))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern
                (Promotion
                 ((pattern (Lit Int 0))
                  (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                 UReal DataOnly))
               (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
              ((pattern
                (Promotion
                 ((pattern (Lit Int 1))
                  (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                 UReal DataOnly))
               (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
           (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
         9)
        (b))
       (((TargetTerm
          ((pattern
            (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
             (((pattern (Var c))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern (Var x))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern
                (Promotion
                 ((pattern (Lit Int 1))
                  (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                 UReal DataOnly))
               (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
           (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
         17)
        (a c))
       (((TargetTerm
          ((pattern
            (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
             (((pattern (Var d))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern (Var c))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
              ((pattern (Var b))
               (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
           (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
         18)
        (b c d))))
     (var_map
      ((a
        (((TargetTerm
           ((pattern
             (FunApp (StanLib Times__ FnPlain AoS)
              (((pattern (Var z))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var e))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          21)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var a))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var b))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          10)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var c))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var x))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          17)))
       (b
        (((TargetTerm
           ((pattern
             (FunApp (StanLib Times__ FnPlain AoS)
              (((pattern (Var z))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var e))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          21)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var a))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var b))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          10)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var b))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 0))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          9)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var d))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var c))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var b))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          18)))
       (c
        (((TargetTerm
           ((pattern
             (FunApp (StanLib Times__ FnPlain AoS)
              (((pattern (Var z))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var e))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          21)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var c))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var x))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          17)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var d))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var c))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var b))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          18)))
       (d
        (((TargetTerm
           ((pattern
             (FunApp (StanLib Times__ FnPlain AoS)
              (((pattern (Var z))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var e))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          21)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var d))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var c))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var b))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          18)))
       (e
        (((TargetTerm
           ((pattern
             (FunApp (StanLib Times__ FnPlain AoS)
              (((pattern (Var z))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var e))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          21)))
       (f
        (((TargetTerm
           ((pattern
             (FunApp (StanLib Times__ FnPlain AoS)
              (((pattern (Var f))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var f))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          21))))))
    |}]

let complex_example =
  Test_utils.mir_of_string
    {|
        data {
          real x;
          real y;
        }
        parameters {
          real a;
          real b;
          real c;
          real d;
          real e;
          real f;
        }
        model
        {
          a ~ normal(0, 1);
          b ~ normal(a, 1);
          x ~ normal(b, 1);
          y ~ normal(c, 1);
          d ~ normal(b, 1);
          e ~ normal(a, 1);
          f ~ normal(a, 1);
          f ~ normal(e, 1);
        }
      |}

let%expect_test "Priors complex example" =
  let priors = list_priors complex_example in
  print_s
    [%sexp
      (String.Map.to_list priors
        : (string
          * ((factor * label) Set.Poly.t option * Middle.Location_span.t))
          list)];
  [%expect
    {|
    ((a
      (((((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var a))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 0))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          9)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var e))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var a))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          14)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var f))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var a))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          15)))
       ((begin_loc
         ((filename string) (line_num 7) (col_num 10) (included_from ())))
        (end_loc
         ((filename string) (line_num 7) (col_num 17) (included_from ()))))))
     (b
      (((((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var b))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var a))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          10)
         ((TargetTerm
           ((pattern
             (FunApp (StanLib normal_lpdf (FnLpdf true) AoS)
              (((pattern (Var d))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern (Var b))
                (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable))))
               ((pattern
                 (Promotion
                  ((pattern (Lit Int 1))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  UReal DataOnly))
                (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UReal) (loc <opaque>) (adlevel AutoDiffable)))))
          13)))
       ((begin_loc
         ((filename string) (line_num 8) (col_num 10) (included_from ())))
        (end_loc
         ((filename string) (line_num 8) (col_num 17) (included_from ()))))))
     (c
      ((())
       ((begin_loc
         ((filename string) (line_num 9) (col_num 10) (included_from ())))
        (end_loc
         ((filename string) (line_num 9) (col_num 17) (included_from ()))))))
     (d
      ((())
       ((begin_loc
         ((filename string) (line_num 10) (col_num 10) (included_from ())))
        (end_loc
         ((filename string) (line_num 10) (col_num 17) (included_from ()))))))
     (e
      ((())
       ((begin_loc
         ((filename string) (line_num 11) (col_num 10) (included_from ())))
        (end_loc
         ((filename string) (line_num 11) (col_num 17) (included_from ()))))))
     (f
      ((())
       ((begin_loc
         ((filename string) (line_num 12) (col_num 10) (included_from ())))
        (end_loc
         ((filename string) (line_num 12) (col_num 17) (included_from ())))))))
    |}]
