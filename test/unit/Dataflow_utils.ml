open Middle
open Analysis_and_optimization.Dataflow_utils
open Std
open Std.Sexp_conv
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
  let block = Stmt.Pattern.Block mir.log_prob in
  let statement_map =
    Stmt.(
      build_statement_map
        (fun {pattern; _} -> pattern)
        (fun {meta; _} -> meta)
        {meta= Location_span.empty; pattern= block}) in
  let exits, preds = build_predecessor_graph statement_map in
  print_s
    [%sexp
      (statement_map
        : ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t)];
  print_s [%sexp (exits : label Set.Poly.t)];
  print_s [%sexp (preds : label Set.Poly.t LabelMap.t)];
  [%expect
    {|
      ((1
        ((Block (2))
         ((begin_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ())))
          (end_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ()))))))
       (2
        ((Block (3))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (byte_num 23)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (byte_num 61)
            (included_from ()))))))
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
           ((filename string) (line_num 3) (col_num 8) (byte_num 23)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (byte_num 61)
            (included_from ()))))))
       (4
        ((Block (5))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (byte_num 48)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (byte_num 61)
            (included_from ()))))))
       (5
        ((NRFunApp (CompilerInternal FnPrint)
          (((pattern
             (FunApp (Operator Plus)
              (((pattern (Lit Int 3))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
               ((pattern (Lit Int 4))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (byte_num 48)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 23) (byte_num 61)
            (included_from ())))))))
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
  let block = Stmt.Pattern.Block mir.log_prob in
  let statement_map =
    Stmt.(
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
  let block = Stmt.Pattern.Block mir.log_prob in
  Stmt.{meta= Location_span.empty; pattern= block}

let example1_statement_map =
  Stmt.(
    build_statement_map
      (fun {pattern; _} -> pattern)
      (fun {meta; _} -> meta)
      example1_program)

let%expect_test "Statement label map example" =
  print_s
    [%sexp
      (LabelMap.map example1_statement_map ~f:fst
        : (Expr.Typed.t, label) Stmt.Pattern.t LabelMap.t)];
  [%expect
    {|
      ((1 (Block (2))) (2 (Block (3 4 5)))
       (3
        (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt))
         (initialize Default)))
       (4
        (Assignment ((LVariable i) ()) UInt
         ((pattern (Lit Int 0))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
       (5
        (IfElse
         ((pattern
           (FunApp (Operator Less)
            (((pattern (Var i))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Lit Int 0))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         6 (8)))
       (6 (Block (7)))
       (7
        (NRFunApp (CompilerInternal FnPrint)
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
           (FunApp (Operator Greater)
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
              (FunApp (Operator Greater)
               (((pattern (Var j))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                ((pattern (Lit Int 8))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern
              (FunApp (Operator Less)
               (((pattern (Var i))
                 (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                ((pattern
                  (FunApp (Operator PMinus)
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
           (FunApp (Operator Greater)
            (((pattern (Var j))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
             ((pattern (Lit Int 5))
              (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
         18 (20)))
       (18 (Block (19))) (19 Continue) (20 (Block (21)))
       (21
        (NRFunApp (CompilerInternal FnPrint)
         (((pattern (Lit Str Badger))
           (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))
          ((pattern
            (FunApp (Operator Plus)
             (((pattern (Var i))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern (Var j))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
       (22
        (NRFunApp (CompilerInternal FnPrint)
         (((pattern (Lit Str Fin))
           (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))))
    |}]

let%expect_test "Predecessor graph example" =
  let exits, preds = build_predecessor_graph example1_statement_map in
  print_s
    [%sexp ((exits, preds) : label Set.Poly.t * label Set.Poly.t LabelMap.t)];
  [%expect
    {|
      ((1)
       ((1 (2)) (2 (6 8)) (3 ()) (4 (3)) (5 (4)) (6 (7)) (7 (5)) (8 (9 13))
        (9 (5 10 16 19)) (10 (22)) (11 (9)) (12 (13)) (13 (11)) (14 (11 12))
        (15 (16)) (16 (14)) (17 (14 15)) (18 (19)) (19 (17)) (20 (21)) (21 (17))
        (22 (18 20))))
    |}]

let%expect_test "Controlflow graph example" =
  let cf = build_cf_graph example1_statement_map in
  print_s [%sexp (cf : label Set.Poly.t LabelMap.t)];
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
      (fun pattern meta -> Stmt.{pattern; meta})
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
    Stmt.(
      Pattern.SList [{pattern= Block mir.log_prob; meta= Location_span.empty}])
  in
  Stmt.{meta= Location_span.empty; pattern= blocks}

let example3_statement_map =
  Stmt.(
    build_statement_map
      (fun {pattern; _} -> pattern)
      (fun {meta; _} -> meta)
      example3_program)

let%expect_test "Statement label map example 3" =
  print_s
    [%sexp
      (example3_statement_map
        : ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t)];
  [%expect
    {|
      ((1
        ((SList (2))
         ((begin_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ())))
          (end_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ()))))))
       (2
        ((Block (3))
         ((begin_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ())))
          (end_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ()))))))
       (3
        ((Block (4 6))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (byte_num 23)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 19) (byte_num 34)
            (included_from ()))))))
       (4
        ((While
          ((pattern (Lit Int 42))
           (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
          5)
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (byte_num 23)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 19) (byte_num 34)
            (included_from ()))))))
       (5
        (Skip
         ((begin_loc
           ((filename string) (line_num 3) (col_num 18) (byte_num 33)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 3) (col_num 19) (byte_num 34)
            (included_from ()))))))
       (6
        ((NRFunApp (CompilerInternal FnPrint)
          (((pattern (Lit Str exit))
            (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))
         ((begin_loc
           ((filename string) (line_num 4) (col_num 8) (byte_num 43)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 22) (byte_num 57)
            (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 3" =
  let cf = build_cf_graph example3_statement_map in
  print_s [%sexp (cf : label Set.Poly.t LabelMap.t)];
  [%expect {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 ()))
    |}]

let%expect_test "Predecessor graph example 3" =
  (* TODO: this is still wrong. The correct answer is
   *     ((6) ((1 ()) (2 (1)) (3 (2)) (4 (3 5)) (5 (4)) (6 (5))))
   * Similarly for for-loops.
   *)
  let exits, preds = build_predecessor_graph example3_statement_map in
  print_s
    [%sexp ((exits, preds) : label Set.Poly.t * label Set.Poly.t LabelMap.t)];
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
    Stmt.(
      Pattern.SList [{pattern= Block mir.log_prob; meta= Location_span.empty}])
  in
  Stmt.{meta= Location_span.empty; pattern= blocks}

let example4_statement_map =
  Stmt.(
    build_statement_map
      (fun {pattern; _} -> pattern)
      (fun {meta; _} -> meta)
      example4_program)

let%expect_test "Statement label map example 4" =
  print_s
    [%sexp
      (example4_statement_map
        : ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t)];
  [%expect
    {|
      ((1
        ((SList (2))
         ((begin_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ())))
          (end_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ()))))))
       (2
        ((Block (3))
         ((begin_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ())))
          (end_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ()))))))
       (3
        ((Block (4))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (byte_num 23)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 9) (byte_num 81)
            (included_from ()))))))
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
           ((filename string) (line_num 3) (col_num 8) (byte_num 23)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 9) (byte_num 81)
            (included_from ()))))))
       (5
        ((Block (6 7))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 23) (byte_num 38)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 9) (byte_num 81)
            (included_from ()))))))
       (6
        (Continue
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (byte_num 50)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 19) (byte_num 59)
            (included_from ()))))))
       (7
        (Skip
         ((begin_loc
           ((filename string) (line_num 5) (col_num 10) (byte_num 70)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 11) (byte_num 71)
            (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 4" =
  let cf = build_cf_graph example4_statement_map in
  print_s [%sexp (cf : label Set.Poly.t LabelMap.t)];
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 ()) (5 (4)) (6 (4)) (7 (4 6)))
    |}]

let%expect_test "Predecessor graph example 4" =
  let exits, preds = build_predecessor_graph example4_statement_map in
  (* TODO: this is still wrong. The correct answer is
   * ( (7) ( (1 ()) (2 (1)) (3 (2)) (4 (3 6)) (5 (4)) (6 (5)) (7 ()) ) )
   * or a very conservative approximation
   * ( (7) ( (1 ()) (2 (1)) (3 (2)) (4 (3 6 7)) (5 (4)) (6 (5)) (7 (6)) ) )
   *)
  print_s
    [%sexp ((exits, preds) : label Set.Poly.t * label Set.Poly.t LabelMap.t)];
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
    Stmt.(
      Pattern.SList [{pattern= Block mir.log_prob; meta= Location_span.empty}])
  in
  Stmt.{meta= Location_span.empty; pattern= blocks}

let example5_statement_map =
  Stmt.(
    build_statement_map
      (fun {pattern; _} -> pattern)
      (fun {meta; _} -> meta)
      example5_program)

let%expect_test "Statement label map example 5" =
  print_s
    [%sexp
      (example5_statement_map
        : ((Expr.Typed.t, label) Stmt.Pattern.t * Location_span.t) LabelMap.t)];
  [%expect
    {|
      ((1
        ((SList (2))
         ((begin_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ())))
          (end_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ()))))))
       (2
        ((Block (3))
         ((begin_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ())))
          (end_loc
           ((filename "") (line_num -1) (col_num -1) (byte_num -1)
            (included_from ()))))))
       (3
        ((Block (4 8))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 8) (byte_num 23)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 9) (byte_num 78)
            (included_from ()))))))
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
           ((filename string) (line_num 3) (col_num 8) (byte_num 23)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 9) (byte_num 78)
            (included_from ()))))))
       (5
        ((Block (6 7))
         ((begin_loc
           ((filename string) (line_num 3) (col_num 23) (byte_num 38)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 6) (col_num 9) (byte_num 78)
            (included_from ()))))))
       (6
        (Break
         ((begin_loc
           ((filename string) (line_num 4) (col_num 10) (byte_num 50)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 4) (col_num 16) (byte_num 56)
            (included_from ()))))))
       (7
        (Skip
         ((begin_loc
           ((filename string) (line_num 5) (col_num 10) (byte_num 67)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 11) (byte_num 68)
            (included_from ()))))))
       (8
        (Skip
         ((begin_loc
           ((filename string) (line_num 7) (col_num 8) (byte_num 87)
            (included_from ())))
          (end_loc
           ((filename string) (line_num 7) (col_num 9) (byte_num 88)
            (included_from ())))))))
    |}]

let%expect_test "Controlflow graph example 5" =
  let cf = build_cf_graph example5_statement_map in
  print_s [%sexp (cf : label Set.Poly.t LabelMap.t)];
  [%expect
    {|
      ((1 ()) (2 ()) (3 ()) (4 (6)) (5 (4)) (6 (4)) (7 (4)) (8 ()))
    |}]

let%expect_test "Predecessor graph example 5" =
  let exits, preds = build_predecessor_graph example5_statement_map in
  (* TODO: this is still very very conservative (e.g. I'd hope for (8) ((1 ()))
     (2 (1)) (3 (2)) (4 (3)) (5 (4)) (6 (5)) (7 ()) (8 (6)) but maybe that's too
     much to ask for ) *)
  print_s
    [%sexp ((exits, preds) : label Set.Poly.t * label Set.Poly.t LabelMap.t)];
  [%expect
    {|
      ((2) ((1 ()) (2 (3)) (3 (8)) (4 (1 5)) (5 (7)) (6 (4)) (7 (6)) (8 (4 6))))
    |}]

let print_control_contexts source =
  let mir = Test_utils.mir_of_string source in
  let bodies =
    List.filter_map mir.functions_block ~f:(fun definition ->
        definition.Program.fdbody) in
  let statements =
    build_statement_map
      (fun Stmt.{pattern; _} -> pattern)
      (fun Stmt.{meta; _} -> meta)
      Stmt.{meta= Location_span.empty; pattern= SList (bodies @ mir.log_prob)}
  in
  let controls_by_statement = build_controls statements in
  let statement_name id =
    match fst (LabelMap.find id statements) with
    | Stmt.Pattern.For {loopvar; _} -> "for " ^ loopvar
    | While (condition, _) -> Fmt.str "while %a" Expr.Typed.pp condition
    | IfElse (condition, _, _) -> Fmt.str "if %a" Expr.Typed.pp condition
    | NRFunApp (CompilerInternal FnPrint, args) ->
        Fmt.str "print(%a)" (Fmt.list ~sep:Fmt.comma Expr.Typed.pp) args
    | Break -> "break"
    | Continue -> "continue"
    | Return (Some result) -> Fmt.str "return %a" Expr.Typed.pp result
    | Return None -> "return"
    | _ -> failwith "Expected a control statement or print" in
  let names ids = List.map ids ~f:statement_name |> String.concat ~sep:"; " in
  LabelMap.iter statements ~f:(fun ~key:id ~data:(pattern, _) ->
      match pattern with
      | Stmt.Pattern.For _ | While _ | NRFunApp (CompilerInternal FnPrint, _) ->
          let statement_controls = LabelMap.find id controls_by_statement in
          let enclosing =
            List.map statement_controls.enclosing ~f:(fun {id= parent; side} ->
                statement_name parent
                ^
                match side with
                | None -> ""
                | Some Then -> " (then)"
                | Some Else -> " (else)")
            |> String.concat ~sep:"; " in
          Fmt.pr
            "%s@.  enclosing: [%s]@.  loops: [%s]@.  control dependencies: \
             [%s]@."
            (statement_name id) enclosing
            (names (enclosing_loop_ids id pattern statement_controls))
            (names
               (Set.Poly.to_list (control_dependency_ids statement_controls)))
      | _ -> ())

let%expect_test
    "Controls: nested for and while loops retain outer-to-inner ancestry" =
  print_control_contexts
    {|data { int<lower=0> N; }
      model {
        for (n in 1:N) {
          int remaining = 2;
          while (remaining > 0) {
            print("inside while");
            remaining -= 1;
          }
          print("after while");
        }
        print("outside loops");
      }|};
  [%expect
    {|
    for n
      enclosing: []
      loops: [for n]
      control dependencies: []
    while (remaining > 0)
      enclosing: [for n]
      loops: [for n; while (remaining > 0)]
      control dependencies: [for n]
    print("inside while")
      enclosing: [for n; while (remaining > 0)]
      loops: [for n; while (remaining > 0)]
      control dependencies: [while (remaining > 0)]
    print("after while")
      enclosing: [for n]
      loops: [for n]
      control dependencies: [for n]
    print("outside loops")
      enclosing: []
      loops: []
      control dependencies: []
    |}]

let%expect_test "Controls: then and else branches stay distinct" =
  print_control_contexts
    {|data { int choose_then; }
      model {
        if (choose_then) print("then branch");
        else print("else branch");
        print("after branches");
      }|};
  [%expect
    {|
    print("then branch")
      enclosing: [if choose_then (then)]
      loops: []
      control dependencies: [if choose_then]
    print("else branch")
      enclosing: [if choose_then (else)]
      loops: []
      control dependencies: [if choose_then]
    print("after branches")
      enclosing: []
      loops: []
      control dependencies: []
    |}]

let%expect_test "Controls: continue controls the rest of the iteration" =
  print_control_contexts
    {|data { int skip; }
      model {
        for (n in 1:3) {
          if (skip) continue;
          print("after continue");
        }
        print("after loop");
      }|};
  [%expect
    {|
    for n
      enclosing: []
      loops: [for n]
      control dependencies: []
    print("after continue")
      enclosing: [for n]
      loops: [for n]
      control dependencies: [for n; continue]
    print("after loop")
      enclosing: []
      loops: []
      control dependencies: []
    |}]

let%expect_test "Controls: break is a dependency of the loop header" =
  print_control_contexts
    {|data { int stop; }
      model {
        for (n in 1:3) {
          if (stop) break;
          print("after break");
        }
        print("after loop");
      }|};
  [%expect
    {|
    for n
      enclosing: []
      loops: [for n]
      control dependencies: [break]
    print("after break")
      enclosing: [for n]
      loops: [for n]
      control dependencies: [for n]
    print("after loop")
      enclosing: []
      loops: []
      control dependencies: []
    |}]

let%expect_test "Controls: return controls later statements in the function" =
  print_control_contexts
    {|functions {
        int f(int stop) {
          if (stop) return 0;
          print("after return");
          return 1;
        }
      } model {}|};
  [%expect
    {|
    print("after return")
      enclosing: []
      loops: []
      control dependencies: [return 0]
    |}]

let%expect_test
    "Controls: an outer continue remains visible inside nested controls" =
  print_control_contexts
    {|data { int skip; int choose_then; }
      model {
        for (n in 1:3) {
          if (skip) continue;
          int remaining = 2;
          while (remaining > 0) {
            if (choose_then) print("nested then");
            else print("nested else");
            remaining -= 1;
          }
        }
      }|};
  [%expect
    {|
    for n
      enclosing: []
      loops: [for n]
      control dependencies: []
    while (remaining > 0)
      enclosing: [for n]
      loops: [for n; while (remaining > 0)]
      control dependencies: [for n; continue]
    print("nested then")
      enclosing: [for n; while (remaining > 0); if choose_then (then)]
      loops: [for n; while (remaining > 0)]
      control dependencies: [continue; if choose_then]
    print("nested else")
      enclosing: [for n; while (remaining > 0); if choose_then (else)]
      loops: [for n; while (remaining > 0)]
      control dependencies: [continue; if choose_then]
    |}]
