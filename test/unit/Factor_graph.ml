open Frontend
open Analysis_and_optimization.Factor_graph
open Core_kernel
open Analysis_and_optimization.Dataflow_types

let reject_example =
  let ast =
    Frontend_utils.typed_ast_of_string_exn
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
  in
  Ast_to_Mir.trans_prog "" ast

let%expect_test "Factor graph reject example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps = prog_factor_graph reject_example in
  print_s [%sexp (deps : factor_graph)] ;
  [%expect
    {|
      ((factor_map
        (((Reject 20) ())
         (((TargetTerm
            ((pattern (Lit Int 1))
             (meta
              ((type_ UInt)
               (loc
                ((begin_loc
                  ((filename string) (line_num 29) (col_num 26) (included_from ())))
                 (end_loc
                  ((filename string) (line_num 29) (col_num 27) (included_from ())))))
               (adlevel DataOnly)))))
           22)
          ())
         (((TargetTerm
            ((pattern (Lit Int 1))
             (meta
              ((type_ UInt)
               (loc
                ((begin_loc
                  ((filename string) (line_num 29) (col_num 30) (included_from ())))
                 (end_loc
                  ((filename string) (line_num 29) (col_num 31) (included_from ())))))
               (adlevel DataOnly)))))
           22)
          ())))
       (var_map ()))
    |}]

let complex_example =
  let ast =
    Frontend_utils.typed_ast_of_string_exn
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
  in
  Ast_to_Mir.trans_prog "" ast

let%expect_test "Factor graph complex example" =
  let deps = prog_factor_graph complex_example in
  print_s [%sexp (deps : factor_graph)] ;
  [%expect
    {|
((factor_map
  ((((TargetTerm
      ((pattern
        (FunApp (StanLib Times__)
         (((pattern (Var f))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 23) (col_num 28)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 23) (col_num 29)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var f))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 23) (col_num 30)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 23) (col_num 31)
                 (included_from ())))))
             (adlevel AutoDiffable)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 23) (col_num 28) (included_from ())))
           (end_loc
            ((filename string) (line_num 23) (col_num 31) (included_from ())))))
         (adlevel AutoDiffable)))))
     21)
    ((VVar f)))
   (((TargetTerm
      ((pattern
        (FunApp (StanLib Times__)
         (((pattern (Var z))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 23) (col_num 20)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 23) (col_num 21)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var e))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 23) (col_num 24)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 23) (col_num 25)
                 (included_from ())))))
             (adlevel AutoDiffable)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 23) (col_num 20) (included_from ())))
           (end_loc
            ((filename string) (line_num 23) (col_num 25) (included_from ())))))
         (adlevel AutoDiffable)))))
     21)
    ((VVar a) (VVar b) (VVar c) (VVar d) (VVar e)))
   (((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var a))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 13) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 13) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var b))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 13) (col_num 21)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 13) (col_num 22)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Lit Int 1))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 13) (col_num 24)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 13) (col_num 25)
                 (included_from ())))))
             (adlevel DataOnly)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 13) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 13) (col_num 27) (included_from ())))))
         (adlevel AutoDiffable)))))
     10)
    ((VVar a) (VVar b)))
   (((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var b))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 12) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 12) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Lit Int 0))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 12) (col_num 21)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 12) (col_num 22)
                 (included_from ())))))
             (adlevel DataOnly))))
          ((pattern (Lit Int 1))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 12) (col_num 24)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 12) (col_num 25)
                 (included_from ())))))
             (adlevel DataOnly)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 12) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 12) (col_num 27) (included_from ())))))
         (adlevel AutoDiffable)))))
     9)
    ((VVar b)))
   (((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var c))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 20) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 20) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var x))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 20) (col_num 22)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 20) (col_num 23)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Lit Int 1))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 20) (col_num 25)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 20) (col_num 26)
                 (included_from ())))))
             (adlevel DataOnly)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 20) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 20) (col_num 28) (included_from ())))))
         (adlevel AutoDiffable)))))
     17)
    ((VVar a) (VVar c)))
   (((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var d))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 21) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 21) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var c))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 21) (col_num 22)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 21) (col_num 23)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var b))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 21) (col_num 25)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 21) (col_num 26)
                 (included_from ())))))
             (adlevel AutoDiffable)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 21) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 21) (col_num 28) (included_from ())))))
         (adlevel AutoDiffable)))))
     18)
    ((VVar b) (VVar c) (VVar d)))))
 (var_map
  (((VVar a)
    (((TargetTerm
       ((pattern
         (FunApp (StanLib Times__)
          (((pattern (Var z))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 20)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 21)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var e))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 24)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 25)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 23) (col_num 20)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 23) (col_num 25)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      21)
     ((TargetTerm
       ((pattern
         (FunApp (StanLib normal_lupdf)
          (((pattern (Var a))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 13) (col_num 10)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 13) (col_num 11)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var b))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 13) (col_num 21)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 13) (col_num 22)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Lit Int 1))
            (meta
             ((type_ UInt)
              (loc
               ((begin_loc
                 ((filename string) (line_num 13) (col_num 24)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 13) (col_num 25)
                  (included_from ())))))
              (adlevel DataOnly)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 13) (col_num 10)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 13) (col_num 27)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      10)
     ((TargetTerm
       ((pattern
         (FunApp (StanLib normal_lupdf)
          (((pattern (Var c))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 20) (col_num 10)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 20) (col_num 11)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var x))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 20) (col_num 22)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 20) (col_num 23)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Lit Int 1))
            (meta
             ((type_ UInt)
              (loc
               ((begin_loc
                 ((filename string) (line_num 20) (col_num 25)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 20) (col_num 26)
                  (included_from ())))))
              (adlevel DataOnly)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 20) (col_num 10)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 20) (col_num 28)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      17)))
   ((VVar b)
    (((TargetTerm
       ((pattern
         (FunApp (StanLib Times__)
          (((pattern (Var z))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 20)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 21)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var e))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 24)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 25)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 23) (col_num 20)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 23) (col_num 25)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      21)
     ((TargetTerm
       ((pattern
         (FunApp (StanLib normal_lupdf)
          (((pattern (Var a))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 13) (col_num 10)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 13) (col_num 11)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var b))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 13) (col_num 21)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 13) (col_num 22)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Lit Int 1))
            (meta
             ((type_ UInt)
              (loc
               ((begin_loc
                 ((filename string) (line_num 13) (col_num 24)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 13) (col_num 25)
                  (included_from ())))))
              (adlevel DataOnly)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 13) (col_num 10)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 13) (col_num 27)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      10)
     ((TargetTerm
       ((pattern
         (FunApp (StanLib normal_lupdf)
          (((pattern (Var b))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 12) (col_num 10)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 12) (col_num 11)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Lit Int 0))
            (meta
             ((type_ UInt)
              (loc
               ((begin_loc
                 ((filename string) (line_num 12) (col_num 21)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 12) (col_num 22)
                  (included_from ())))))
              (adlevel DataOnly))))
           ((pattern (Lit Int 1))
            (meta
             ((type_ UInt)
              (loc
               ((begin_loc
                 ((filename string) (line_num 12) (col_num 24)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 12) (col_num 25)
                  (included_from ())))))
              (adlevel DataOnly)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 12) (col_num 10)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 12) (col_num 27)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      9)
     ((TargetTerm
       ((pattern
         (FunApp (StanLib normal_lupdf)
          (((pattern (Var d))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 10)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 11)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var c))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 22)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 23)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var b))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 25)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 26)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 21) (col_num 10)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 21) (col_num 28)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      18)))
   ((VVar c)
    (((TargetTerm
       ((pattern
         (FunApp (StanLib Times__)
          (((pattern (Var z))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 20)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 21)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var e))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 24)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 25)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 23) (col_num 20)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 23) (col_num 25)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      21)
     ((TargetTerm
       ((pattern
         (FunApp (StanLib normal_lupdf)
          (((pattern (Var c))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 20) (col_num 10)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 20) (col_num 11)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var x))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 20) (col_num 22)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 20) (col_num 23)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Lit Int 1))
            (meta
             ((type_ UInt)
              (loc
               ((begin_loc
                 ((filename string) (line_num 20) (col_num 25)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 20) (col_num 26)
                  (included_from ())))))
              (adlevel DataOnly)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 20) (col_num 10)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 20) (col_num 28)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      17)
     ((TargetTerm
       ((pattern
         (FunApp (StanLib normal_lupdf)
          (((pattern (Var d))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 10)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 11)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var c))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 22)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 23)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var b))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 25)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 26)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 21) (col_num 10)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 21) (col_num 28)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      18)))
   ((VVar d)
    (((TargetTerm
       ((pattern
         (FunApp (StanLib Times__)
          (((pattern (Var z))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 20)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 21)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var e))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 24)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 25)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 23) (col_num 20)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 23) (col_num 25)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      21)
     ((TargetTerm
       ((pattern
         (FunApp (StanLib normal_lupdf)
          (((pattern (Var d))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 10)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 11)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var c))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 22)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 23)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var b))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 21) (col_num 25)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 21) (col_num 26)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 21) (col_num 10)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 21) (col_num 28)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      18)))
   ((VVar e)
    (((TargetTerm
       ((pattern
         (FunApp (StanLib Times__)
          (((pattern (Var z))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 20)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 21)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var e))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 24)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 25)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 23) (col_num 20)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 23) (col_num 25)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      21)))
   ((VVar f)
    (((TargetTerm
       ((pattern
         (FunApp (StanLib Times__)
          (((pattern (Var f))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 28)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 29)
                  (included_from ())))))
              (adlevel AutoDiffable))))
           ((pattern (Var f))
            (meta
             ((type_ UReal)
              (loc
               ((begin_loc
                 ((filename string) (line_num 23) (col_num 30)
                  (included_from ())))
                (end_loc
                 ((filename string) (line_num 23) (col_num 31)
                  (included_from ())))))
              (adlevel AutoDiffable)))))))
        (meta
         ((type_ UReal)
          (loc
           ((begin_loc
             ((filename string) (line_num 23) (col_num 28)
              (included_from ())))
            (end_loc
             ((filename string) (line_num 23) (col_num 31)
              (included_from ())))))
          (adlevel AutoDiffable)))))
      21))))))
    |}]

let complex_example =
  let ast =
    Frontend_utils.typed_ast_of_string_exn
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
  in
  Ast_to_Mir.trans_prog "" ast

let%expect_test "Priors complex example" =
  let priors = list_priors complex_example in
  print_s
    [%sexp (priors : (vexpr, (factor * label) Set.Poly.t option) Map.Poly.t)] ;
  [%expect
    {|
(((VVar a)
  ((((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var a))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 16) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 16) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Lit Int 0))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 16) (col_num 21)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 16) (col_num 22)
                 (included_from ())))))
             (adlevel DataOnly))))
          ((pattern (Lit Int 1))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 16) (col_num 24)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 16) (col_num 25)
                 (included_from ())))))
             (adlevel DataOnly)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 16) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 16) (col_num 27) (included_from ())))))
         (adlevel AutoDiffable)))))
     9)
    ((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var e))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 21) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 21) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var a))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 21) (col_num 21)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 21) (col_num 22)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Lit Int 1))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 21) (col_num 24)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 21) (col_num 25)
                 (included_from ())))))
             (adlevel DataOnly)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 21) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 21) (col_num 27) (included_from ())))))
         (adlevel AutoDiffable)))))
     14)
    ((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var f))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 22) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 22) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var a))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 22) (col_num 21)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 22) (col_num 22)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Lit Int 1))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 22) (col_num 24)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 22) (col_num 25)
                 (included_from ())))))
             (adlevel DataOnly)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 22) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 22) (col_num 27) (included_from ())))))
         (adlevel AutoDiffable)))))
     15))))
 ((VVar b)
  ((((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var b))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 17) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 17) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var a))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 17) (col_num 21)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 17) (col_num 22)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Lit Int 1))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 17) (col_num 24)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 17) (col_num 25)
                 (included_from ())))))
             (adlevel DataOnly)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 17) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 17) (col_num 27) (included_from ())))))
         (adlevel AutoDiffable)))))
     10)
    ((TargetTerm
      ((pattern
        (FunApp (StanLib normal_lupdf)
         (((pattern (Var d))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 20) (col_num 10)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 20) (col_num 11)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Var b))
           (meta
            ((type_ UReal)
             (loc
              ((begin_loc
                ((filename string) (line_num 20) (col_num 21)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 20) (col_num 22)
                 (included_from ())))))
             (adlevel AutoDiffable))))
          ((pattern (Lit Int 1))
           (meta
            ((type_ UInt)
             (loc
              ((begin_loc
                ((filename string) (line_num 20) (col_num 24)
                 (included_from ())))
               (end_loc
                ((filename string) (line_num 20) (col_num 25)
                 (included_from ())))))
             (adlevel DataOnly)))))))
       (meta
        ((type_ UReal)
         (loc
          ((begin_loc
            ((filename string) (line_num 20) (col_num 10) (included_from ())))
           (end_loc
            ((filename string) (line_num 20) (col_num 27) (included_from ())))))
         (adlevel AutoDiffable)))))
     13))))
 ((VVar c) (())) ((VVar d) (())) ((VVar e) (())) ((VVar f) (())))
    |}]
