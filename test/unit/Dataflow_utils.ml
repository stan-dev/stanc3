open Frontend
open Middle
open Analysis_and_optimization.Dataflow_utils
open Core_kernel
open Analysis_and_optimization.Dataflow_types

let semantic_check_program ast =
  Option.value_exn
    (Result.ok
       (Semantic_check.semantic_check_program
          (Option.value_exn (Result.ok ast))))

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
  let mir = Ast_to_Mir.trans_prog "" (semantic_check_program ast) in
  let block = Stmt.Fixed.Pattern.Block mir.log_prob in
  let statement_map =
    Stmt.Fixed.(
      build_statement_map
        (fun {pattern; _} -> pattern)
        (fun {meta; _} -> meta)
        {meta= Location_span.empty; pattern= block})
  in
  let exits, preds = build_predecessor_graph statement_map in
  print_s
    [%sexp
      ( statement_map
        : ( label
          , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * Location_span.t )
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
           ((pattern (Lit Int 1))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
          (upper
           ((pattern (Lit Int 2))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
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
          (((pattern
             (FunApp StanLib Plus__
              (((pattern (Lit Int 3))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
               ((pattern (Lit Int 4))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
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
  let mir = Ast_to_Mir.trans_prog "" (semantic_check_program ast) in
  let block = Stmt.Fixed.Pattern.Block mir.log_prob in
  let statement_map =
    Stmt.Fixed.(
      build_statement_map
        (fun {pattern; _} -> pattern)
        (fun {meta; _} -> meta)
        {meta= Location_span.empty; pattern= block})
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
  let mir = Ast_to_Mir.trans_prog "" (semantic_check_program ast) in
  let block = Stmt.Fixed.Pattern.Block mir.log_prob in
  Stmt.Fixed.{meta= Location_span.empty; pattern= block}

let example1_statement_map =
  Stmt.Fixed.(
    build_statement_map
      (fun {pattern; _} -> pattern)
      (fun {meta; _} -> meta)
      example1_program)

let%expect_test "Statement label map example" =
  print_s
    [%sexp
      ( Map.Poly.map example1_statement_map ~f:fst
        : (label, (Expr.Typed.t, label) Stmt.Fixed.Pattern.t) Map.Poly.t )] ;
  [%expect
    {|
      ((1 (Block (2))) (2 (Block (3 4 5)))
       (3 (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt))))
       (4
        (Assignment (i UInt ())
         ((pattern (Lit Int 0))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
       (5
        (IfElse
         ((pattern
           (FunApp StanLib Less__
            (((pattern (Var i))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Lit Int 0))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         6 (8)))
       (6 (Block (7)))
       (7
        (NRFunApp CompilerInternal FnPrint__
         (((pattern (Var i))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
       (8 (Block (9)))
       (9
        (For (loopvar j)
         (lower
          ((pattern (Lit Int 1))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
         (upper
          ((pattern (Lit Int 10))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
         (body 10)))
       (10 (Block (11 14 17 22)))
       (11
        (IfElse
         ((pattern
           (FunApp StanLib Greater__
            (((pattern (Var j))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Lit Int 9))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         12 ()))
       (12 (Block (13))) (13 Break)
       (14
        (IfElse
         ((pattern
           (EAnd
            ((pattern
              (FunApp StanLib Greater__
               (((pattern (Var j))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                ((pattern (Lit Int 8))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern
              (FunApp StanLib Less__
               (((pattern (Var i))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                ((pattern
                  (FunApp StanLib PMinus__
                   (((pattern (Lit Int 1))
                     (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         15 ()))
       (15 (Block (16))) (16 Continue)
       (17
        (IfElse
         ((pattern
           (FunApp StanLib Greater__
            (((pattern (Var j))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Lit Int 5))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         18 (20)))
       (18 (Block (19))) (19 Continue) (20 (Block (21)))
       (21
        (NRFunApp CompilerInternal FnPrint__
         (((pattern (Lit Str Badger))
           (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
          ((pattern
            (FunApp StanLib Plus__
             (((pattern (Var i))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern (Var j))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
       (22
        (NRFunApp CompilerInternal FnPrint__
         (((pattern (Lit Str Fin))
           (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))))
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
      (fun pattern meta -> Stmt.Fixed.{pattern; meta})
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
  let mir = Ast_to_Mir.trans_prog "" (semantic_check_program ast) in
  let blocks =
    Stmt.Fixed.(
      Pattern.SList [{pattern= Block mir.log_prob; meta= Location_span.empty}])
  in
  Stmt.Fixed.{meta= Location_span.empty; pattern= blocks}

let example3_statement_map =
  Stmt.Fixed.(
    build_statement_map
      (fun {pattern; _} -> pattern)
      (fun {meta; _} -> meta)
      example3_program)

let%expect_test "Statement label map example 3" =
  print_s
    [%sexp
      ( example3_statement_map
        : ( label
          , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * Location_span.t )
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
        ((While
          ((pattern (Lit Int 42))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
          5)
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
        ((NRFunApp CompilerInternal FnPrint__
          (((pattern (Lit Str exit))
            (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))
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
  let mir = Ast_to_Mir.trans_prog "" (semantic_check_program ast) in
  let blocks =
    Stmt.Fixed.(
      Pattern.SList [{pattern= Block mir.log_prob; meta= Location_span.empty}])
  in
  Stmt.Fixed.{meta= Location_span.empty; pattern= blocks}

let example4_statement_map =
  Stmt.Fixed.(
    build_statement_map
      (fun {pattern; _} -> pattern)
      (fun {meta; _} -> meta)
      example4_program)

let%expect_test "Statement label map example 4" =
  print_s
    [%sexp
      ( example4_statement_map
        : ( label
          , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * Location_span.t )
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
        ((For (loopvar i)
          (lower
           ((pattern (Lit Int 1))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
          (upper
           ((pattern (Lit Int 6))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
          (body 5))
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
  let mir = Ast_to_Mir.trans_prog "" (semantic_check_program ast) in
  let blocks =
    Stmt.Fixed.(
      Pattern.SList [{pattern= Block mir.log_prob; meta= Location_span.empty}])
  in
  Stmt.Fixed.{meta= Location_span.empty; pattern= blocks}

let example5_statement_map =
  Stmt.Fixed.(
    build_statement_map
      (fun {pattern; _} -> pattern)
      (fun {meta; _} -> meta)
      example5_program)

let%expect_test "Statement label map example 5" =
  print_s
    [%sexp
      ( example5_statement_map
        : ( label
          , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * Location_span.t )
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
        ((For (loopvar i)
          (lower
           ((pattern (Lit Int 1))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
          (upper
           ((pattern (Lit Int 6))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
          (body 5))
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
