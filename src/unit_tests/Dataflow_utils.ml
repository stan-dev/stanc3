open Frontend
open Middle
open Analysis_and_optimization.Dataflow_utils
open Core_kernel
open Analysis_and_optimization.Dataflow_types

(***********************************)
(* Tests                           *)
(***********************************)

let%expect_test "Loop test" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        for (i in 1:2)
          print(3 + 4);
      }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let block = Middle.Block mir.log_prob in
  let statement_map =
    build_statement_map
      (fun s -> s.stmt)
      (fun s -> s.smeta)
      {stmt= block; smeta= Middle.no_span}
  in
  let exits, preds = build_predecessor_graph statement_map in
  print_s
    [%sexp
      ( statement_map
        : ( label
          , (mtype_loc_ad with_expr, label) statement * location_span )
          Map.Poly.t )] ;
  print_s [%sexp (exits : label Set.Poly.t)] ;
  print_s [%sexp (preds : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1
        ((Block (2))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((Block (3))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (included_from ()))))))
       (3
        ((For (loopvar i)
          (lower
           ((expr (Lit Int 1))
            (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
          (upper
           ((expr (Lit Int 2))
            (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
          (body 4))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (included_from ()))))))
       (4
        ((Block (5))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (included_from ()))))))
       (5
        ((NRFunApp CompilerInternal FnPrint__
          (((expr
             (FunApp StanLib Plus__
              (((expr (Lit Int 3))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
               ((expr (Lit Int 4))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
            (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (included_from ())))))))
      (3)
      ((1 ()) (2 (1)) (3 (2 5)) (4 (3)) (5 (4)))
    |}]

let%expect_test "Loop passthrough" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        model {
          if (1) {
            if (1) {
              for (j in 1:2) {
                print("Badger", j);
              }
            } else {
              for (j in 2:1) {
                print("Badger", j);
              }
            }
          } else {
            if (1) {
              while (1) {
                print("Badger");
              }
            } else {
              while (0) {
                print("Badger");
              }
            }
          }
        }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let block = Middle.Block mir.log_prob in
  let statement_map =
    build_statement_map
      (fun s -> s.stmt)
      (fun s -> s.smeta)
      {stmt= block; smeta= Middle.no_span}
  in
  let exits, _ = build_predecessor_graph statement_map in
  print_s [%sexp (exits : label Set.Poly.t)] ;
  [%expect {|
      (7 11 17 21)
    |}]

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
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let block = Middle.Block mir.log_prob in
  {stmt= block; smeta= Middle.no_span}

let example1_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.smeta) example1_program

let%expect_test "Statement label map example" =
  print_s
    [%sexp
      ( Map.Poly.map example1_statement_map ~f:fst
        : (label, (expr_typed_located, label) statement) Map.Poly.t )] ;
  [%expect
    {|
      ((1 (Block (2))) (2 (Block (3 4 5)))
       (3 (Decl (decl_adtype DataOnly) (decl_id i) (decl_type (Sized SInt))))
       (4 (Assignment (i ()) (Lit Int 0)))
       (5 (IfElse (FunApp StanLib Less__ ((Var i) (Lit Int 0))) 6 (8)))
       (6 (Block (7))) (7 (NRFunApp CompilerInternal FnPrint__ ((Var i))))
       (8 (Block (9)))
       (9 (For (loopvar j) (lower (Lit Int 1)) (upper (Lit Int 10)) (body 10)))
       (10 (Block (11 14 17 22)))
       (11 (IfElse (FunApp StanLib Greater__ ((Var j) (Lit Int 9))) 12 ()))
       (12 (Block (13))) (13 Break)
       (14
        (IfElse
         (EAnd (FunApp StanLib Greater__ ((Var j) (Lit Int 8)))
          (FunApp StanLib Less__ ((Var i) (FunApp StanLib PMinus__ ((Lit Int 1))))))
         15 ()))
       (15 (Block (16))) (16 Continue)
       (17 (IfElse (FunApp StanLib Greater__ ((Var j) (Lit Int 5))) 18 (20)))
       (18 (Block (19))) (19 Continue) (20 (Block (21)))
       (21
        (NRFunApp CompilerInternal FnPrint__
         ((Lit Str Badger) (FunApp StanLib Plus__ ((Var i) (Var j))))))
       (22 (NRFunApp CompilerInternal FnPrint__ ((Lit Str Fin)))))
    |}]

let%expect_test "Predecessor graph example" =
  let exits, preds = build_predecessor_graph example1_statement_map in
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((7 9 13)
       ((1 ()) (2 (1)) (3 (2)) (4 (3)) (5 (4)) (6 (5)) (7 (6)) (8 (5))
        (9 (8 16 19 22)) (10 (9)) (11 (10)) (12 (11)) (13 (12)) (14 (13)) (15 (14))
        (16 (15)) (17 (16)) (18 (17)) (19 (18)) (20 (17)) (21 (20)) (22 (19 21))))
    |}]

let%expect_test "Controlflow graph example" =
  let cf = build_cf_graph example1_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 ()) (6 (5)) (7 (5)) (8 (5)) (9 (5 13))
       (10 (9)) (11 (9)) (12 (11)) (13 (11)) (14 (9)) (15 (14)) (16 (14))
       (17 (9 16)) (18 (16 17)) (19 (16 17)) (20 (16 17)) (21 (16 17))
       (22 (9 16 19)))
    |}]

let%test "Reconstructed recursive statement" =
  let stmt =
    build_recursive_statement
      (fun stmt meta -> {stmt; smeta= meta})
      example1_statement_map 1
  in
  stmt = example1_program

let example3_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        while (42);
        print("exit");
      }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let blocks =
    Middle.SList [{stmt= Block mir.log_prob; smeta= Middle.no_span}]
  in
  {stmt= blocks; smeta= Middle.no_span}

let example3_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.smeta) example3_program

let%expect_test "Statement label map example 3" =
  print_s
    [%sexp
      ( example3_statement_map
        : ( label
          , (expr_typed_located, label) statement * Middle.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((SList (2))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((Block (3))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (3
        ((Block (4 6))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 19) (included_from ()))))))
       (4
        ((While (Lit Int 42) 5)
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 19) (included_from ()))))))
       (5
        (Skip
         ((begin_loc
           ((filename string) (line_num 3) (col_num 18) (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 19) (included_from ()))))))
       (6
        ((NRFunApp CompilerInternal FnPrint__ ((Lit Str exit)))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 22) (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 3" =
  let cf = build_cf_graph example3_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 ()))
    |}]

let%expect_test "Predecessor graph example 3" =
  (* TODO: this is still wrong. The correct answer is
      ((6) ((1 ()) (2 (1)) (3 (2)) (4 (3 5)) (5 (4)) (6 (5))))
  Similarly for for-loops.
  ) *)
  let exits, preds = build_predecessor_graph example3_statement_map in
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((6) ((1 ()) (2 (1)) (3 (2)) (4 (3 5)) (5 (4)) (6 (4))))
    |}]

let example4_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        for (i in 1:6) {
          continue;
          ;
        }
      }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let blocks =
    Middle.SList [{stmt= Block mir.log_prob; smeta= Middle.no_span}]
  in
  {stmt= blocks; smeta= Middle.no_span}

let example4_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.smeta) example4_program

let%expect_test "Statement label map example 4" =
  print_s
    [%sexp
      ( example4_statement_map
        : ( label
          , (expr_typed_located, label) statement * Middle.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((SList (2))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((Block (3))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (3
        ((Block (4))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (4
        ((For (loopvar i) (lower (Lit Int 1)) (upper (Lit Int 6)) (body 5))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (5
        ((Block (6 7))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 23) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (6
        (Continue
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 19) (included_from ()))))))
       (7
        (Skip
         ((begin_loc
           ((filename string) (line_num 5) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 11) (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 4" =
  let cf = build_cf_graph example4_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 (4)) (7 (4 6)))
    |}]

let%expect_test "Predecessor graph example 4" =
  let exits, preds = build_predecessor_graph example4_statement_map in
  (* TODO: this is still wrong. The correct answer is
  ( (7) ( (1 ()) (2 (1)) (3 (2)) (4 (3 6)) (5 (4)) (6 (5)) (7 ()) ) )
  or a very conservative approximation
  ( (7) ( (1 ()) (2 (1)) (3 (2)) (4 (3 6 7)) (5 (4)) (6 (5)) (7 (6)) ) )
   *)
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((4) ((1 ()) (2 (1)) (3 (2)) (4 (3 6 7)) (5 (4)) (6 (5)) (7 (6))))
    |}]

let example5_program =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
      model {
        for (i in 1:6) {
          break;
          ;
        }
        ;
      }
      |}
  in
  let mir =
    Ast_to_Mir.trans_prog "" (Semantic_check.semantic_check_program ast)
  in
  let blocks =
    Middle.SList [{stmt= Block mir.log_prob; smeta= Middle.no_span}]
  in
  {stmt= blocks; smeta= Middle.no_span}

let example5_statement_map =
  build_statement_map (fun s -> s.stmt) (fun s -> s.smeta) example5_program

let%expect_test "Statement label map example 5" =
  print_s
    [%sexp
      ( example5_statement_map
        : ( label
          , (expr_typed_located, label) statement * Middle.location_span )
          Map.Poly.t )] ;
  [%expect
    {|
      ((1
        ((SList (2))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (2
        ((Block (3))
         ((begin_loc ((filename "") (line_num 0) (col_num 0) (included_from ())))
          (end_loc ((filename "") (line_num 0) (col_num 0) (included_from ()))))))
       (3
        ((Block (4 8))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (4
        ((For (loopvar i) (lower (Lit Int 1)) (upper (Lit Int 6)) (body 5))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (5
        ((Block (6 7))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 23) (included_from ())))
          (end_loc ((filename string) (line_num 6) (col_num 9) (included_from ()))))))
       (6
        (Break
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 16) (included_from ()))))))
       (7
        (Skip
         ((begin_loc
           ((filename string) (line_num 5) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 11) (included_from ()))))))
       (8
        (Skip
         ((begin_loc
           ((filename string) (line_num 7) (col_num 8) (included_from ())))
          (end_loc ((filename string) (line_num 7) (col_num 9) (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 5" =
  let cf = build_cf_graph example5_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 (6)) (5 (4)) (6 (4)) (7 (4)) (8 ()))
    |}]

let%expect_test "Predecessor graph example 5" =
  let exits, preds = build_predecessor_graph example5_statement_map in
  (* TODO: this is still very very conservative (e.g. I'd hope for 
  (8) ((1 ())) (2 (1)) (3 (2)) (4 (3)) (5 (4)) (6 (5)) (7 ()) (8 (6))
  but maybe that's too much to ask for
  ) *)
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)] ;
  [%expect
    {|
      ((8) ((1 ()) (2 (1)) (3 (2)) (4 (3 7)) (5 (4)) (6 (5)) (7 (6)) (8 (4 6))))
    |}]
