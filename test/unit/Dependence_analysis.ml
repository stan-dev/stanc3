open Core_kernel
open Frontend
open Analysis_and_optimization.Dependence_analysis
open Analysis_and_optimization.Monotone_framework
open Middle
open Analysis_and_optimization.Dataflow_types
open Analysis_and_optimization.Dataflow_utils

let semantic_check_program ast =
  Option.value_exn
    (Result.ok
       (Semantic_check.semantic_check_program
          (Option.value_exn (Result.ok ast))))

let example1_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Dependency graph example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps = log_prob_dependency_graph example1_program in
  print_s [%sexp (deps : (label, label Set.Poly.t) Map.Poly.t)] ;
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
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    Map.Poly.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) ->
        ( reaching_defn_lookup x.reaching_defn_entry (VVar "j")
        , reaching_defn_lookup x.reaching_defn_exit (VVar "j") ) )
  in
  print_s
    [%sexp (deps : (label, label Set.Poly.t * label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 (() ())) (2 (() ())) (3 (() ())) (4 (() ())) (5 (() ())) (6 (() ()))
       (7 (() ())) (8 (() ())) (9 ((9) (9))) (10 ((9) (9))) (11 ((9) (9)))
       (12 ((9) (9))) (13 ((9) (9))) (14 ((9) (9))) (15 ((9) (9))) (16 ((9) (9)))
       (17 ((9) (9))) (18 ((9) (9))) (19 ((9) (9))) (20 ((9) (9))) (21 ((9) (9)))
       (22 ((9) (9))))
    |}]

let%expect_test "Reaching defns example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    Map.Poly.map (log_prob_build_dep_info_map example1_program)
      ~f:(fun (_, x) -> (x.reaching_defn_entry, x.reaching_defn_exit) )
  in
  print_s
    [%sexp
      ( deps
        : ( label
          , reaching_defn Set.Poly.t * reaching_defn Set.Poly.t )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1 (() ())) (2 (() ())) (3 (() (((VVar i) 3))))
       (4 ((((VVar i) 3)) (((VVar i) 4)))) (5 ((((VVar i) 4)) (((VVar i) 4))))
       (6 ((((VVar i) 4)) (((VVar i) 4)))) (7 ((((VVar i) 4)) (((VVar i) 4))))
       (8 ((((VVar i) 4)) (((VVar i) 4))))
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
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps =
    node_vars_dependencies
      (log_prob_build_dep_info_map example1_program)
      (Set.Poly.singleton (VVar "j"))
      17
  in
  print_s [%sexp (deps : label Set.Poly.t)] ;
  [%expect {|
      (4 5 9 11 13 14 16)
    |}]

let uninitialized_var_example =
  let ast =
    Parse.parse_string Parser.Incremental.program
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
  in
  Ast_to_Mir.trans_prog "" (semantic_check_program ast)

let%expect_test "Uninitialized variables example" =
  (*let deps = snd (build_predecessor_graph example1_statement_map) in*)
  let deps = mir_uninitialized_variables uninitialized_var_example in
  print_s [%sexp (deps : (location_span * string) Set.Poly.t)] ;
  [%expect
    {|
      ((((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
         (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))
        k)
       (((begin_loc
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

let%expect_test "Show Uninitialized variables example" =
  print_s [%sexp (uninitialized_var_example : typed_prog)] ;
  [%expect
    {|
      ((functions_block
        (((fdrt (UInt)) (fdname f) (fdargs ((AutoDiffable y UInt)))
          (fdbody
           ((stmt
             (Block
              (((stmt
                 (Decl (decl_adtype AutoDiffable) (decl_id x)
                  (decl_type (Sized SInt))))
                (smeta <opaque>))
               ((stmt
                 (IfElse
                  ((expr
                    (FunApp StanLib Greater__
                     (((expr (Var y))
                       (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                      ((expr (Lit Int 2))
                       (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                  ((stmt
                    (Return
                     (((expr
                        (FunApp StanLib Plus__
                         (((expr (Var y))
                           (emeta
                            ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                          ((expr (Lit Int 24))
                           (emeta
                            ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                       (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                   (smeta <opaque>))
                  ()))
                (smeta <opaque>))
               ((stmt
                 (Return
                  (((expr
                     (FunApp StanLib Plus__
                      (((expr (Var y))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                       ((expr (Lit Int 2))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                (smeta <opaque>)))))
            (smeta <opaque>)))
          (fdloc <opaque>))))
       (input_vars ((w SReal)))
       (prepare_data
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id w) (decl_type (Sized SReal))))
          (smeta <opaque>))
         ((stmt
           (Assignment (w UReal ())
            ((expr
              (FunApp CompilerInternal FnReadData__
               (((expr (Lit Str w))
                 (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))
                ((expr (Lit Str scalar))
                 (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
             (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))
         ((stmt
           (Decl (decl_adtype DataOnly) (decl_id wu) (decl_type (Sized SReal))))
          (smeta <opaque>))
         ((stmt
           (NRFunApp CompilerInternal FnPrint__
            (((expr (Var wu))
              (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
          (smeta <opaque>))
         ((stmt
           (NRFunApp CompilerInternal FnPrint__
            (((expr (Var w))
              (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
          (smeta <opaque>))
         ((stmt
           (Assignment (wu UReal ())
            ((expr (Var w))
             (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))))
       (log_prob
        (((stmt
           (Decl (decl_adtype AutoDiffable) (decl_id x) (decl_type (Sized SReal))))
          (smeta <opaque>))
         ((stmt
           (Assignment (x UReal ())
            ((expr
              (FunApp CompilerInternal FnReadParam__
               (((expr (Lit Str x))
                 (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))
                ((expr (Lit Str scalar))
                 (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
             (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable))))))
          (smeta <opaque>))
         ((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id i)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id z)
                (decl_type (Sized SInt))))
              (smeta <opaque>))
             ((stmt
               (Assignment (z UInt ())
                ((expr (Lit Int 0))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var z))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var x))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
              (smeta <opaque>))
             ((stmt
               (IfElse
                ((expr
                  (FunApp StanLib Equals__
                   (((expr (Var z))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                    ((expr (Lit Int 1))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (Assignment (i UInt ())
                       ((expr (Lit Int 1))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                     (smeta <opaque>)))))
                 (smeta <opaque>))
                (((stmt (Block ())) (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>))
             ((stmt
               (IfElse
                ((expr
                  (FunApp StanLib Equals__
                   (((expr (Var z))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                    ((expr (Lit Int 2))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                 (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                ((stmt
                  (Block
                   (((stmt
                      (Assignment (i UInt ())
                       ((expr (Lit Int 1))
                        (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                     (smeta <opaque>)))))
                 (smeta <opaque>))
                (((stmt
                   (Block
                    (((stmt
                       (Assignment (i UInt ())
                        ((expr (Lit Int 2))
                         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
                      (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>))
             ((stmt
               (NRFunApp CompilerInternal FnPrint__
                (((expr (Var i))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id x) (decl_type (Sized SReal))))
          (smeta <opaque>))
         ((stmt
           (Assignment (x UReal ())
            ((expr
              (FunApp CompilerInternal FnReadParam__
               (((expr (Lit Str x))
                 (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))
                ((expr (Lit Str scalar))
                 (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
             (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))
         ((stmt
           (NRFunApp CompilerInternal FnWriteParam__
            (((expr (Var x))
              (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
          (smeta <opaque>))
         ((stmt
           (IfElse
            ((expr (Var emit_generated_quantities__))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
            ((stmt
              (Block
               (((stmt
                  (Decl (decl_adtype DataOnly) (decl_id k)
                   (decl_type (Sized SInt))))
                 (smeta <opaque>))
                ((stmt
                  (NRFunApp CompilerInternal FnPrint__
                   (((expr (Var k))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                 (smeta <opaque>))
                ((stmt
                  (NRFunApp CompilerInternal FnWriteParam__
                   (((expr (Var k))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                 (smeta <opaque>)))))
             (smeta <opaque>))
            ()))
          (smeta <opaque>))))
       (transform_inits
        (((stmt
           (Decl (decl_adtype DataOnly) (decl_id x) (decl_type (Sized SReal))))
          (smeta <opaque>))
         ((stmt
           (Assignment (x UReal ())
            ((expr
              (FunApp CompilerInternal FnReadData__
               (((expr (Lit Str x))
                 (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))
                ((expr (Lit Str scalar))
                 (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
             (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))))
          (smeta <opaque>))
         ((stmt
           (NRFunApp CompilerInternal FnWriteParam__
            (((expr (Var x))
              (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
          (smeta <opaque>))))
       (output_vars
        ((x
          ((out_unconstrained_st SReal) (out_constrained_st SReal)
           (out_block Parameters)))
         (k
          ((out_unconstrained_st SInt) (out_constrained_st SInt)
           (out_block GeneratedQuantities)))))
       (prog_name "") (prog_path ""))
    |}]
