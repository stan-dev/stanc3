open Middle
open Analysis_and_optimization.Dataflow_utils
open Core
open Core.Poly
open Analysis_and_optimization.Dataflow_types

(***********************************)
(* Tests                           *)
(***********************************)

let%expect_test "Loop test" =
  let mir =
    Test_utils.mir_of_string
      {|
      model {
        for (i in 1:2)
          print(3 + 4);
      }
      |}
  in
  let block = Stmt.Fixed.Pattern.Block mir.log_prob in
  let statement_map =
    Stmt.Fixed.(
      build_statement_map
        (fun {pattern; _} -> pattern)
        (fun {meta; _} -> meta)
        {meta= Location_span.empty; pattern= block}) in
  let exits, preds = build_predecessor_graph statement_map in
  print_s
    [%sexp
      (statement_map
        : ( label
          , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * Location_span.t )
          Map.Poly.t)];
  print_s [%sexp (exits : label Set.Poly.t)];
  print_s [%sexp (preds : (label, label Set.Poly.t) Map.Poly.t)];
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
        ((NRFunApp (CompilerInternal FnPrint)
          (((pattern
             (FunApp (StanLib Plus__ FnPlain AoS)
              (((pattern (Lit Int 3))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
               ((pattern (Lit Int 4))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (included_from ())))))))
      (1)
      ((1 (2)) (2 (3)) (3 (4)) (4 (5)) (5 (3)))
    |}]

let%expect_test "Loop passthrough" =
  let mir =
    Test_utils.mir_of_string
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
  let block = Stmt.Fixed.Pattern.Block mir.log_prob in
  let statement_map =
    Stmt.Fixed.(
      build_statement_map
        (fun {pattern; _} -> pattern)
        (fun {meta; _} -> meta)
        {meta= Location_span.empty; pattern= block}) in
  let exits, _ = build_predecessor_graph statement_map in
  print_s [%sexp (exits : label Set.Poly.t)];
  [%expect {|
      (1)
    |}]

let example1_program =
  let mir =
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
  in
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
      (Map.Poly.map example1_statement_map ~f:fst
        : (label, (Expr.Typed.t, label) Stmt.Fixed.Pattern.t) Map.Poly.t)];
  [%expect
    {|
      ((1 (Block (2))) (2 (Block (3 4)))
       (3
        (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt))
         (initialize true)
         (assignment
          (((pattern (Lit Int 0))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))))
       (4
        (IfElse
         ((pattern
           (FunApp (StanLib Less__ FnPlain AoS)
            (((pattern (Var i))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Lit Int 0))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         5 (7)))
       (5 (Block (6)))
       (6
        (NRFunApp (CompilerInternal FnPrint)
         (((pattern (Var i))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
       (7 (Block (8)))
       (8
        (For (loopvar j)
         (lower
          ((pattern (Lit Int 1))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
         (upper
          ((pattern (Lit Int 10))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
         (body 9)))
       (9 (Block (10 13 16 21)))
       (10
        (IfElse
         ((pattern
           (FunApp (StanLib Greater__ FnPlain AoS)
            (((pattern (Var j))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Lit Int 9))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         11 ()))
       (11 (Block (12))) (12 Break)
       (13
        (IfElse
         ((pattern
           (EAnd
            ((pattern
              (FunApp (StanLib Greater__ FnPlain AoS)
               (((pattern (Var j))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                ((pattern (Lit Int 8))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern
              (FunApp (StanLib Less__ FnPlain AoS)
               (((pattern (Var i))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                ((pattern
                  (FunApp (StanLib PMinus__ FnPlain AoS)
                   (((pattern (Lit Int 1))
                     (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         14 ()))
       (14 (Block (15))) (15 Continue)
       (16
        (IfElse
         ((pattern
           (FunApp (StanLib Greater__ FnPlain AoS)
            (((pattern (Var j))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Lit Int 5))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         17 (19)))
       (17 (Block (18))) (18 Continue) (19 (Block (20)))
       (20
        (NRFunApp (CompilerInternal FnPrint)
         (((pattern (Lit Str Badger))
           (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
          ((pattern
            (FunApp (StanLib Plus__ FnPlain AoS)
             (((pattern (Var i))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern (Var j))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
       (21
        (NRFunApp (CompilerInternal FnPrint)
         (((pattern (Lit Str Fin))
           (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))))
    |}]

let%expect_test "Predecessor graph example" =
  let exits, preds = build_predecessor_graph example1_statement_map in
  print_s
    [%sexp
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)];
  [%expect
    {|
      ((1)
       ((1 (2)) (2 (5 7)) (3 ()) (4 (3)) (5 (6)) (6 (4)) (7 (8 12)) (8 (4 9 15 18))
        (9 (21)) (10 (8)) (11 (12)) (12 (10)) (13 (10 11)) (14 (15)) (15 (13))
        (16 (13 14)) (17 (18)) (18 (16)) (19 (20)) (20 (16)) (21 (17 19))))
    |}]

let%expect_test "Controlflow graph example" =
  let cf = build_cf_graph example1_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)];
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 (4)) (7 (4)) (8 (4 12)) (9 (8))
       (10 (8)) (11 (10)) (12 (10)) (13 (8)) (14 (13)) (15 (13)) (16 (8 15))
       (17 (15 16)) (18 (15 16)) (19 (15 16)) (20 (15 16)) (21 (8 15 18)))
    |}]

let%test "Reconstructed recursive statement" =
  let stmt =
    build_recursive_statement
      (fun pattern meta -> Stmt.Fixed.{pattern; meta})
      example1_statement_map 1 in
  stmt = example1_program

let example3_program =
  let mir =
    Test_utils.mir_of_string
      {|
      model {
        while (42);
        print("exit");
      }
      |}
  in
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
      (example3_statement_map
        : ( label
          , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * Location_span.t )
          Map.Poly.t)];
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
        ((NRFunApp (CompilerInternal FnPrint)
          (((pattern (Lit Str exit))
            (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 8) (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 22) (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 3" =
  let cf = build_cf_graph example3_statement_map in
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)];
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
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)];
  [%expect
    {|
      ((2) ((1 ()) (2 (3)) (3 (6)) (4 (1 5)) (5 (4)) (6 (4))))
    |}]

let example4_program =
  let mir =
    Test_utils.mir_of_string
      {|
      model {
        for (i in 1:6) {
          continue;
          ;
        }
      }
      |}
  in
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
      (example4_statement_map
        : ( label
          , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * Location_span.t )
          Map.Poly.t)];
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
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)];
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
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)];
  [%expect
    {|
      ((2) ((1 ()) (2 (3)) (3 (4)) (4 (1 5 6)) (5 (7)) (6 (4)) (7 (6))))
    |}]

let example5_program =
  let mir =
    Test_utils.mir_of_string
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
      (example5_statement_map
        : ( label
          , (Expr.Typed.t, label) Stmt.Fixed.Pattern.t * Location_span.t )
          Map.Poly.t)];
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
  print_s [%sexp (cf : (label, label Set.Poly.t) Map.Poly.t)];
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
      ((exits, preds) : label Set.Poly.t * (label, label Set.Poly.t) Map.Poly.t)];
  [%expect
    {|
      ((2) ((1 ()) (2 (3)) (3 (8)) (4 (1 5)) (5 (7)) (6 (4)) (7 (6)) (8 (4 6))))
    |}]
