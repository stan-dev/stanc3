open Core_kernel
open Frontend

let print_ast_of_string s =
  let ast =
    Frontend_utils.untyped_ast_of_string s
    |> Result.map_error ~f:Middle.Errors.to_string
    |> Result.ok_or_failwith
  in
  print_s [%sexp (ast : Ast.untyped_program)]

(* TESTS *)
let%expect_test "parse conditional" =
  print_ast_of_string "model { if (1 < 2) { print(\"hi\");}}" ;
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      ((((stmt
          (IfThenElse
           ((expr
             (BinOp
              ((expr (IntNumeral 1))
               (emeta
                ((loc
                  ((begin_loc
                    ((filename string) (line_num 1) (col_num 12)
                     (included_from ())))
                   (end_loc
                    ((filename string) (line_num 1) (col_num 13)
                     (included_from ()))))))))
              Less
              ((expr (IntNumeral 2))
               (emeta
                ((loc
                  ((begin_loc
                    ((filename string) (line_num 1) (col_num 16)
                     (included_from ())))
                   (end_loc
                    ((filename string) (line_num 1) (col_num 17)
                     (included_from ()))))))))))
            (emeta
             ((loc
               ((begin_loc
                 ((filename string) (line_num 1) (col_num 12) (included_from ())))
                (end_loc
                 ((filename string) (line_num 1) (col_num 17) (included_from ()))))))))
           ((stmt
             (Block
              (((stmt (Print ((PString "\"hi\""))))
                (smeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 1) (col_num 21)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 1) (col_num 33)
                      (included_from ())))))))))))
            (smeta
             ((loc
               ((begin_loc
                 ((filename string) (line_num 1) (col_num 19) (included_from ())))
                (end_loc
                 ((filename string) (line_num 1) (col_num 34) (included_from ()))))))))
           ()))
         (smeta
          ((loc
            ((begin_loc
              ((filename string) (line_num 1) (col_num 8) (included_from ())))
             (end_loc
              ((filename string) (line_num 1) (col_num 34) (included_from ())))))))))))
     (generatedquantitiesblock ())) |}]

let%expect_test "parse dangling else problem" =
  print_ast_of_string
    "model { if (1 < 2) print(\"I'm sorry\"); if (2 < 3) print(\", Dave, \"); \
     else print(\"I'm afraid I can't do that.\");}" ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt
            (IfThenElse
             ((expr
               (BinOp
                ((expr (IntNumeral 1))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 1) (col_num 12)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 1) (col_num 13)
                       (included_from ()))))))))
                Less
                ((expr (IntNumeral 2))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 1) (col_num 16)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 1) (col_num 17)
                       (included_from ()))))))))))
              (emeta
               ((loc
                 ((begin_loc
                   ((filename string) (line_num 1) (col_num 12) (included_from ())))
                  (end_loc
                   ((filename string) (line_num 1) (col_num 17) (included_from ()))))))))
             ((stmt (Print ((PString "\"I'm sorry\""))))
              (smeta
               ((loc
                 ((begin_loc
                   ((filename string) (line_num 1) (col_num 19) (included_from ())))
                  (end_loc
                   ((filename string) (line_num 1) (col_num 38) (included_from ()))))))))
             ()))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 1) (col_num 8) (included_from ())))
               (end_loc
                ((filename string) (line_num 1) (col_num 38) (included_from ()))))))))
          ((stmt
            (IfThenElse
             ((expr
               (BinOp
                ((expr (IntNumeral 2))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 1) (col_num 43)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 1) (col_num 44)
                       (included_from ()))))))))
                Less
                ((expr (IntNumeral 3))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 1) (col_num 47)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 1) (col_num 48)
                       (included_from ()))))))))))
              (emeta
               ((loc
                 ((begin_loc
                   ((filename string) (line_num 1) (col_num 43) (included_from ())))
                  (end_loc
                   ((filename string) (line_num 1) (col_num 48) (included_from ()))))))))
             ((stmt (Print ((PString "\", Dave, \""))))
              (smeta
               ((loc
                 ((begin_loc
                   ((filename string) (line_num 1) (col_num 50) (included_from ())))
                  (end_loc
                   ((filename string) (line_num 1) (col_num 68) (included_from ()))))))))
             (((stmt (Print ((PString "\"I'm afraid I can't do that.\""))))
               (smeta
                ((loc
                  ((begin_loc
                    ((filename string) (line_num 1) (col_num 74)
                     (included_from ())))
                   (end_loc
                    ((filename string) (line_num 1) (col_num 111)
                     (included_from ())))))))))))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 1) (col_num 39) (included_from ())))
               (end_loc
                ((filename string) (line_num 1) (col_num 111) (included_from ())))))))))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse minus unary" =
  print_ast_of_string "model { real x; x = -x;}" ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt
            (VarDecl (decl_type (Sized SReal)) (transformation Identity)
             (identifier
              ((name x)
               (id_loc
                ((begin_loc
                  ((filename string) (line_num 1) (col_num 13) (included_from ())))
                 (end_loc
                  ((filename string) (line_num 1) (col_num 14) (included_from ())))))))
             (initial_value ()) (is_global false)))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 1) (col_num 8) (included_from ())))
               (end_loc
                ((filename string) (line_num 1) (col_num 15) (included_from ()))))))))
          ((stmt
            (Assignment
             (assign_lhs
              ((lval
                (LVariable
                 ((name x)
                  (id_loc
                   ((begin_loc
                     ((filename string) (line_num 1) (col_num 16)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 1) (col_num 17)
                      (included_from ()))))))))
               (lmeta
                ((loc
                  ((begin_loc
                    ((filename string) (line_num 1) (col_num 16)
                     (included_from ())))
                   (end_loc
                    ((filename string) (line_num 1) (col_num 17)
                     (included_from ())))))))))
             (assign_op Assign)
             (assign_rhs
              ((expr
                (PrefixOp PMinus
                 ((expr
                   (Variable
                    ((name x)
                     (id_loc
                      ((begin_loc
                        ((filename string) (line_num 1) (col_num 21)
                         (included_from ())))
                       (end_loc
                        ((filename string) (line_num 1) (col_num 22)
                         (included_from ()))))))))
                  (emeta
                   ((loc
                     ((begin_loc
                       ((filename string) (line_num 1) (col_num 21)
                        (included_from ())))
                      (end_loc
                       ((filename string) (line_num 1) (col_num 22)
                        (included_from ()))))))))))
               (emeta
                ((loc
                  ((begin_loc
                    ((filename string) (line_num 1) (col_num 20)
                     (included_from ())))
                   (end_loc
                    ((filename string) (line_num 1) (col_num 22)
                     (included_from ())))))))))))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 1) (col_num 16) (included_from ())))
               (end_loc
                ((filename string) (line_num 1) (col_num 23) (included_from ())))))))))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse unary over binary" =
  print_ast_of_string "model { real x = x - - x - - x; }" ;
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      ((((stmt
          (VarDecl (decl_type (Sized SReal)) (transformation Identity)
           (identifier
            ((name x)
             (id_loc
              ((begin_loc
                ((filename string) (line_num 1) (col_num 13) (included_from ())))
               (end_loc
                ((filename string) (line_num 1) (col_num 14) (included_from ())))))))
           (initial_value
            (((expr
               (BinOp
                ((expr
                  (BinOp
                   ((expr
                     (Variable
                      ((name x)
                       (id_loc
                        ((begin_loc
                          ((filename string) (line_num 1) (col_num 17)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 1) (col_num 18)
                           (included_from ()))))))))
                    (emeta
                     ((loc
                       ((begin_loc
                         ((filename string) (line_num 1) (col_num 17)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 1) (col_num 18)
                          (included_from ()))))))))
                   Minus
                   ((expr
                     (PrefixOp PMinus
                      ((expr
                        (Variable
                         ((name x)
                          (id_loc
                           ((begin_loc
                             ((filename string) (line_num 1) (col_num 23)
                              (included_from ())))
                            (end_loc
                             ((filename string) (line_num 1) (col_num 24)
                              (included_from ()))))))))
                       (emeta
                        ((loc
                          ((begin_loc
                            ((filename string) (line_num 1) (col_num 23)
                             (included_from ())))
                           (end_loc
                            ((filename string) (line_num 1) (col_num 24)
                             (included_from ()))))))))))
                    (emeta
                     ((loc
                       ((begin_loc
                         ((filename string) (line_num 1) (col_num 21)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 1) (col_num 24)
                          (included_from ()))))))))))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 1) (col_num 17)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 1) (col_num 24)
                       (included_from ()))))))))
                Minus
                ((expr
                  (PrefixOp PMinus
                   ((expr
                     (Variable
                      ((name x)
                       (id_loc
                        ((begin_loc
                          ((filename string) (line_num 1) (col_num 29)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 1) (col_num 30)
                           (included_from ()))))))))
                    (emeta
                     ((loc
                       ((begin_loc
                         ((filename string) (line_num 1) (col_num 29)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 1) (col_num 30)
                          (included_from ()))))))))))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 1) (col_num 27)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 1) (col_num 30)
                       (included_from ()))))))))))
              (emeta
               ((loc
                 ((begin_loc
                   ((filename string) (line_num 1) (col_num 17)
                    (included_from ())))
                  (end_loc
                   ((filename string) (line_num 1) (col_num 30)
                    (included_from ()))))))))))
           (is_global false)))
         (smeta
          ((loc
            ((begin_loc
              ((filename string) (line_num 1) (col_num 8) (included_from ())))
             (end_loc
              ((filename string) (line_num 1) (col_num 31) (included_from ())))))))))))
     (generatedquantitiesblock ())) |}]

let%expect_test "parse indices, two different colons" =
  print_ast_of_string "model { matrix[5, 5] x; print(x[2 - 3 ? 3 : 4 : 2]); }" ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt
            (VarDecl
             (decl_type
              (Sized
               (SMatrix SoA
                ((expr (IntNumeral 5))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 1) (col_num 15)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 1) (col_num 16)
                       (included_from ()))))))))
                ((expr (IntNumeral 5))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 1) (col_num 18)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 1) (col_num 19)
                       (included_from ())))))))))))
             (transformation Identity)
             (identifier
              ((name x)
               (id_loc
                ((begin_loc
                  ((filename string) (line_num 1) (col_num 21) (included_from ())))
                 (end_loc
                  ((filename string) (line_num 1) (col_num 22) (included_from ())))))))
             (initial_value ()) (is_global false)))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 1) (col_num 8) (included_from ())))
               (end_loc
                ((filename string) (line_num 1) (col_num 23) (included_from ()))))))))
          ((stmt
            (Print
             ((PExpr
               ((expr
                 (Indexed
                  ((expr
                    (Variable
                     ((name x)
                      (id_loc
                       ((begin_loc
                         ((filename string) (line_num 1) (col_num 30)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 1) (col_num 31)
                          (included_from ()))))))))
                   (emeta
                    ((loc
                      ((begin_loc
                        ((filename string) (line_num 1) (col_num 30)
                         (included_from ())))
                       (end_loc
                        ((filename string) (line_num 1) (col_num 31)
                         (included_from ()))))))))
                  ((Between
                    ((expr
                      (TernaryIf
                       ((expr
                         (BinOp
                          ((expr (IntNumeral 2))
                           (emeta
                            ((loc
                              ((begin_loc
                                ((filename string) (line_num 1) (col_num 32)
                                 (included_from ())))
                               (end_loc
                                ((filename string) (line_num 1) (col_num 33)
                                 (included_from ()))))))))
                          Minus
                          ((expr (IntNumeral 3))
                           (emeta
                            ((loc
                              ((begin_loc
                                ((filename string) (line_num 1) (col_num 36)
                                 (included_from ())))
                               (end_loc
                                ((filename string) (line_num 1) (col_num 37)
                                 (included_from ()))))))))))
                        (emeta
                         ((loc
                           ((begin_loc
                             ((filename string) (line_num 1) (col_num 32)
                              (included_from ())))
                            (end_loc
                             ((filename string) (line_num 1) (col_num 37)
                              (included_from ()))))))))
                       ((expr (IntNumeral 3))
                        (emeta
                         ((loc
                           ((begin_loc
                             ((filename string) (line_num 1) (col_num 40)
                              (included_from ())))
                            (end_loc
                             ((filename string) (line_num 1) (col_num 41)
                              (included_from ()))))))))
                       ((expr (IntNumeral 4))
                        (emeta
                         ((loc
                           ((begin_loc
                             ((filename string) (line_num 1) (col_num 44)
                              (included_from ())))
                            (end_loc
                             ((filename string) (line_num 1) (col_num 45)
                              (included_from ()))))))))))
                     (emeta
                      ((loc
                        ((begin_loc
                          ((filename string) (line_num 1) (col_num 32)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 1) (col_num 45)
                           (included_from ()))))))))
                    ((expr (IntNumeral 2))
                     (emeta
                      ((loc
                        ((begin_loc
                          ((filename string) (line_num 1) (col_num 48)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 1) (col_num 49)
                           (included_from ()))))))))))))
                (emeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 1) (col_num 30)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 1) (col_num 50)
                      (included_from ()))))))))))))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 1) (col_num 24) (included_from ())))
               (end_loc
                ((filename string) (line_num 1) (col_num 52) (included_from ())))))))))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse operator precedence" =
  print_ast_of_string
    "model {  \
     print({a,b?c:d||e&&f==g!=h<=i<j>=k>l+m-n*o/p%q.*s./t\\r^u[v]'}); }" ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt
            (Print
             ((PExpr
               ((expr
                 (ArrayExpr
                  (((expr
                     (Variable
                      ((name a)
                       (id_loc
                        ((begin_loc
                          ((filename string) (line_num 1) (col_num 16)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 1) (col_num 17)
                           (included_from ()))))))))
                    (emeta
                     ((loc
                       ((begin_loc
                         ((filename string) (line_num 1) (col_num 16)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 1) (col_num 17)
                          (included_from ()))))))))
                   ((expr
                     (TernaryIf
                      ((expr
                        (Variable
                         ((name b)
                          (id_loc
                           ((begin_loc
                             ((filename string) (line_num 1) (col_num 18)
                              (included_from ())))
                            (end_loc
                             ((filename string) (line_num 1) (col_num 19)
                              (included_from ()))))))))
                       (emeta
                        ((loc
                          ((begin_loc
                            ((filename string) (line_num 1) (col_num 18)
                             (included_from ())))
                           (end_loc
                            ((filename string) (line_num 1) (col_num 19)
                             (included_from ()))))))))
                      ((expr
                        (Variable
                         ((name c)
                          (id_loc
                           ((begin_loc
                             ((filename string) (line_num 1) (col_num 20)
                              (included_from ())))
                            (end_loc
                             ((filename string) (line_num 1) (col_num 21)
                              (included_from ()))))))))
                       (emeta
                        ((loc
                          ((begin_loc
                            ((filename string) (line_num 1) (col_num 20)
                             (included_from ())))
                           (end_loc
                            ((filename string) (line_num 1) (col_num 21)
                             (included_from ()))))))))
                      ((expr
                        (BinOp
                         ((expr
                           (Variable
                            ((name d)
                             (id_loc
                              ((begin_loc
                                ((filename string) (line_num 1) (col_num 22)
                                 (included_from ())))
                               (end_loc
                                ((filename string) (line_num 1) (col_num 23)
                                 (included_from ()))))))))
                          (emeta
                           ((loc
                             ((begin_loc
                               ((filename string) (line_num 1) (col_num 22)
                                (included_from ())))
                              (end_loc
                               ((filename string) (line_num 1) (col_num 23)
                                (included_from ()))))))))
                         Or
                         ((expr
                           (BinOp
                            ((expr
                              (Variable
                               ((name e)
                                (id_loc
                                 ((begin_loc
                                   ((filename string) (line_num 1) (col_num 25)
                                    (included_from ())))
                                  (end_loc
                                   ((filename string) (line_num 1) (col_num 26)
                                    (included_from ()))))))))
                             (emeta
                              ((loc
                                ((begin_loc
                                  ((filename string) (line_num 1) (col_num 25)
                                   (included_from ())))
                                 (end_loc
                                  ((filename string) (line_num 1) (col_num 26)
                                   (included_from ()))))))))
                            And
                            ((expr
                              (BinOp
                               ((expr
                                 (BinOp
                                  ((expr
                                    (Variable
                                     ((name f)
                                      (id_loc
                                       ((begin_loc
                                         ((filename string) (line_num 1)
                                          (col_num 28) (included_from ())))
                                        (end_loc
                                         ((filename string) (line_num 1)
                                          (col_num 29) (included_from ()))))))))
                                   (emeta
                                    ((loc
                                      ((begin_loc
                                        ((filename string) (line_num 1)
                                         (col_num 28) (included_from ())))
                                       (end_loc
                                        ((filename string) (line_num 1)
                                         (col_num 29) (included_from ()))))))))
                                  Equals
                                  ((expr
                                    (Variable
                                     ((name g)
                                      (id_loc
                                       ((begin_loc
                                         ((filename string) (line_num 1)
                                          (col_num 31) (included_from ())))
                                        (end_loc
                                         ((filename string) (line_num 1)
                                          (col_num 32) (included_from ()))))))))
                                   (emeta
                                    ((loc
                                      ((begin_loc
                                        ((filename string) (line_num 1)
                                         (col_num 31) (included_from ())))
                                       (end_loc
                                        ((filename string) (line_num 1)
                                         (col_num 32) (included_from ()))))))))))
                                (emeta
                                 ((loc
                                   ((begin_loc
                                     ((filename string) (line_num 1) (col_num 28)
                                      (included_from ())))
                                    (end_loc
                                     ((filename string) (line_num 1) (col_num 32)
                                      (included_from ()))))))))
                               NEquals
                               ((expr
                                 (BinOp
                                  ((expr
                                    (BinOp
                                     ((expr
                                       (BinOp
                                        ((expr
                                          (BinOp
                                           ((expr
                                             (Variable
                                              ((name h)
                                               (id_loc
                                                ((begin_loc
                                                  ((filename string) (line_num 1)
                                                   (col_num 34) (included_from ())))
                                                 (end_loc
                                                  ((filename string) (line_num 1)
                                                   (col_num 35) (included_from ()))))))))
                                            (emeta
                                             ((loc
                                               ((begin_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 34) (included_from ())))
                                                (end_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 35) (included_from ()))))))))
                                           Leq
                                           ((expr
                                             (Variable
                                              ((name i)
                                               (id_loc
                                                ((begin_loc
                                                  ((filename string) (line_num 1)
                                                   (col_num 37) (included_from ())))
                                                 (end_loc
                                                  ((filename string) (line_num 1)
                                                   (col_num 38) (included_from ()))))))))
                                            (emeta
                                             ((loc
                                               ((begin_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 37) (included_from ())))
                                                (end_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 38) (included_from ()))))))))))
                                         (emeta
                                          ((loc
                                            ((begin_loc
                                              ((filename string) (line_num 1)
                                               (col_num 34) (included_from ())))
                                             (end_loc
                                              ((filename string) (line_num 1)
                                               (col_num 38) (included_from ()))))))))
                                        Less
                                        ((expr
                                          (Variable
                                           ((name j)
                                            (id_loc
                                             ((begin_loc
                                               ((filename string) (line_num 1)
                                                (col_num 39) (included_from ())))
                                              (end_loc
                                               ((filename string) (line_num 1)
                                                (col_num 40) (included_from ()))))))))
                                         (emeta
                                          ((loc
                                            ((begin_loc
                                              ((filename string) (line_num 1)
                                               (col_num 39) (included_from ())))
                                             (end_loc
                                              ((filename string) (line_num 1)
                                               (col_num 40) (included_from ()))))))))))
                                      (emeta
                                       ((loc
                                         ((begin_loc
                                           ((filename string) (line_num 1)
                                            (col_num 34) (included_from ())))
                                          (end_loc
                                           ((filename string) (line_num 1)
                                            (col_num 40) (included_from ()))))))))
                                     Geq
                                     ((expr
                                       (Variable
                                        ((name k)
                                         (id_loc
                                          ((begin_loc
                                            ((filename string) (line_num 1)
                                             (col_num 42) (included_from ())))
                                           (end_loc
                                            ((filename string) (line_num 1)
                                             (col_num 43) (included_from ()))))))))
                                      (emeta
                                       ((loc
                                         ((begin_loc
                                           ((filename string) (line_num 1)
                                            (col_num 42) (included_from ())))
                                          (end_loc
                                           ((filename string) (line_num 1)
                                            (col_num 43) (included_from ()))))))))))
                                   (emeta
                                    ((loc
                                      ((begin_loc
                                        ((filename string) (line_num 1)
                                         (col_num 34) (included_from ())))
                                       (end_loc
                                        ((filename string) (line_num 1)
                                         (col_num 43) (included_from ()))))))))
                                  Greater
                                  ((expr
                                    (BinOp
                                     ((expr
                                       (BinOp
                                        ((expr
                                          (Variable
                                           ((name l)
                                            (id_loc
                                             ((begin_loc
                                               ((filename string) (line_num 1)
                                                (col_num 44) (included_from ())))
                                              (end_loc
                                               ((filename string) (line_num 1)
                                                (col_num 45) (included_from ()))))))))
                                         (emeta
                                          ((loc
                                            ((begin_loc
                                              ((filename string) (line_num 1)
                                               (col_num 44) (included_from ())))
                                             (end_loc
                                              ((filename string) (line_num 1)
                                               (col_num 45) (included_from ()))))))))
                                        Plus
                                        ((expr
                                          (Variable
                                           ((name m)
                                            (id_loc
                                             ((begin_loc
                                               ((filename string) (line_num 1)
                                                (col_num 46) (included_from ())))
                                              (end_loc
                                               ((filename string) (line_num 1)
                                                (col_num 47) (included_from ()))))))))
                                         (emeta
                                          ((loc
                                            ((begin_loc
                                              ((filename string) (line_num 1)
                                               (col_num 46) (included_from ())))
                                             (end_loc
                                              ((filename string) (line_num 1)
                                               (col_num 47) (included_from ()))))))))))
                                      (emeta
                                       ((loc
                                         ((begin_loc
                                           ((filename string) (line_num 1)
                                            (col_num 44) (included_from ())))
                                          (end_loc
                                           ((filename string) (line_num 1)
                                            (col_num 47) (included_from ()))))))))
                                     Minus
                                     ((expr
                                       (BinOp
                                        ((expr
                                          (BinOp
                                           ((expr
                                             (BinOp
                                              ((expr
                                                (BinOp
                                                 ((expr
                                                   (BinOp
                                                    ((expr
                                                      (Variable
                                                       ((name n)
                                                        (id_loc
                                                         ((begin_loc
                                                           ((filename string)
                                                            (line_num 1)
                                                            (col_num 48)
                                                            (included_from ())))
                                                          (end_loc
                                                           ((filename string)
                                                            (line_num 1)
                                                            (col_num 49)
                                                            (included_from ()))))))))
                                                     (emeta
                                                      ((loc
                                                        ((begin_loc
                                                          ((filename string)
                                                           (line_num 1)
                                                           (col_num 48)
                                                           (included_from ())))
                                                         (end_loc
                                                          ((filename string)
                                                           (line_num 1)
                                                           (col_num 49)
                                                           (included_from ()))))))))
                                                    Times
                                                    ((expr
                                                      (Variable
                                                       ((name o)
                                                        (id_loc
                                                         ((begin_loc
                                                           ((filename string)
                                                            (line_num 1)
                                                            (col_num 50)
                                                            (included_from ())))
                                                          (end_loc
                                                           ((filename string)
                                                            (line_num 1)
                                                            (col_num 51)
                                                            (included_from ()))))))))
                                                     (emeta
                                                      ((loc
                                                        ((begin_loc
                                                          ((filename string)
                                                           (line_num 1)
                                                           (col_num 50)
                                                           (included_from ())))
                                                         (end_loc
                                                          ((filename string)
                                                           (line_num 1)
                                                           (col_num 51)
                                                           (included_from ()))))))))))
                                                  (emeta
                                                   ((loc
                                                     ((begin_loc
                                                       ((filename string)
                                                        (line_num 1) (col_num 48)
                                                        (included_from ())))
                                                      (end_loc
                                                       ((filename string)
                                                        (line_num 1) (col_num 51)
                                                        (included_from ()))))))))
                                                 Divide
                                                 ((expr
                                                   (Variable
                                                    ((name p)
                                                     (id_loc
                                                      ((begin_loc
                                                        ((filename string)
                                                         (line_num 1) (col_num 52)
                                                         (included_from ())))
                                                       (end_loc
                                                        ((filename string)
                                                         (line_num 1) (col_num 53)
                                                         (included_from ()))))))))
                                                  (emeta
                                                   ((loc
                                                     ((begin_loc
                                                       ((filename string)
                                                        (line_num 1) (col_num 52)
                                                        (included_from ())))
                                                      (end_loc
                                                       ((filename string)
                                                        (line_num 1) (col_num 53)
                                                        (included_from ()))))))))))
                                               (emeta
                                                ((loc
                                                  ((begin_loc
                                                    ((filename string) (line_num 1)
                                                     (col_num 48)
                                                     (included_from ())))
                                                   (end_loc
                                                    ((filename string) (line_num 1)
                                                     (col_num 53)
                                                     (included_from ()))))))))
                                              Modulo
                                              ((expr
                                                (Variable
                                                 ((name q)
                                                  (id_loc
                                                   ((begin_loc
                                                     ((filename string)
                                                      (line_num 1) (col_num 54)
                                                      (included_from ())))
                                                    (end_loc
                                                     ((filename string)
                                                      (line_num 1) (col_num 55)
                                                      (included_from ()))))))))
                                               (emeta
                                                ((loc
                                                  ((begin_loc
                                                    ((filename string) (line_num 1)
                                                     (col_num 54)
                                                     (included_from ())))
                                                   (end_loc
                                                    ((filename string) (line_num 1)
                                                     (col_num 55)
                                                     (included_from ()))))))))))
                                            (emeta
                                             ((loc
                                               ((begin_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 48) (included_from ())))
                                                (end_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 55) (included_from ()))))))))
                                           EltTimes
                                           ((expr
                                             (Variable
                                              ((name s)
                                               (id_loc
                                                ((begin_loc
                                                  ((filename string) (line_num 1)
                                                   (col_num 57) (included_from ())))
                                                 (end_loc
                                                  ((filename string) (line_num 1)
                                                   (col_num 58) (included_from ()))))))))
                                            (emeta
                                             ((loc
                                               ((begin_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 57) (included_from ())))
                                                (end_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 58) (included_from ()))))))))))
                                         (emeta
                                          ((loc
                                            ((begin_loc
                                              ((filename string) (line_num 1)
                                               (col_num 48) (included_from ())))
                                             (end_loc
                                              ((filename string) (line_num 1)
                                               (col_num 58) (included_from ()))))))))
                                        EltDivide
                                        ((expr
                                          (BinOp
                                           ((expr
                                             (Variable
                                              ((name t)
                                               (id_loc
                                                ((begin_loc
                                                  ((filename string) (line_num 1)
                                                   (col_num 60) (included_from ())))
                                                 (end_loc
                                                  ((filename string) (line_num 1)
                                                   (col_num 61) (included_from ()))))))))
                                            (emeta
                                             ((loc
                                               ((begin_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 60) (included_from ())))
                                                (end_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 61) (included_from ()))))))))
                                           LDivide
                                           ((expr
                                             (BinOp
                                              ((expr
                                                (Variable
                                                 ((name r)
                                                  (id_loc
                                                   ((begin_loc
                                                     ((filename string)
                                                      (line_num 1) (col_num 62)
                                                      (included_from ())))
                                                    (end_loc
                                                     ((filename string)
                                                      (line_num 1) (col_num 63)
                                                      (included_from ()))))))))
                                               (emeta
                                                ((loc
                                                  ((begin_loc
                                                    ((filename string) (line_num 1)
                                                     (col_num 62)
                                                     (included_from ())))
                                                   (end_loc
                                                    ((filename string) (line_num 1)
                                                     (col_num 63)
                                                     (included_from ()))))))))
                                              Pow
                                              ((expr
                                                (PostfixOp
                                                 ((expr
                                                   (Indexed
                                                    ((expr
                                                      (Variable
                                                       ((name u)
                                                        (id_loc
                                                         ((begin_loc
                                                           ((filename string)
                                                            (line_num 1)
                                                            (col_num 64)
                                                            (included_from ())))
                                                          (end_loc
                                                           ((filename string)
                                                            (line_num 1)
                                                            (col_num 65)
                                                            (included_from ()))))))))
                                                     (emeta
                                                      ((loc
                                                        ((begin_loc
                                                          ((filename string)
                                                           (line_num 1)
                                                           (col_num 64)
                                                           (included_from ())))
                                                         (end_loc
                                                          ((filename string)
                                                           (line_num 1)
                                                           (col_num 65)
                                                           (included_from ()))))))))
                                                    ((Single
                                                      ((expr
                                                        (Variable
                                                         ((name v)
                                                          (id_loc
                                                           ((begin_loc
                                                             ((filename string)
                                                              (line_num 1)
                                                              (col_num 66)
                                                              (included_from ())))
                                                            (end_loc
                                                             ((filename string)
                                                              (line_num 1)
                                                              (col_num 67)
                                                              (included_from ()))))))))
                                                       (emeta
                                                        ((loc
                                                          ((begin_loc
                                                            ((filename string)
                                                             (line_num 1)
                                                             (col_num 66)
                                                             (included_from ())))
                                                           (end_loc
                                                            ((filename string)
                                                             (line_num 1)
                                                             (col_num 67)
                                                             (included_from ()))))))))))))
                                                  (emeta
                                                   ((loc
                                                     ((begin_loc
                                                       ((filename string)
                                                        (line_num 1) (col_num 64)
                                                        (included_from ())))
                                                      (end_loc
                                                       ((filename string)
                                                        (line_num 1) (col_num 68)
                                                        (included_from ()))))))))
                                                 Transpose))
                                               (emeta
                                                ((loc
                                                  ((begin_loc
                                                    ((filename string) (line_num 1)
                                                     (col_num 64)
                                                     (included_from ())))
                                                   (end_loc
                                                    ((filename string) (line_num 1)
                                                     (col_num 69)
                                                     (included_from ()))))))))))
                                            (emeta
                                             ((loc
                                               ((begin_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 62) (included_from ())))
                                                (end_loc
                                                 ((filename string) (line_num 1)
                                                  (col_num 69) (included_from ()))))))))))
                                         (emeta
                                          ((loc
                                            ((begin_loc
                                              ((filename string) (line_num 1)
                                               (col_num 60) (included_from ())))
                                             (end_loc
                                              ((filename string) (line_num 1)
                                               (col_num 69) (included_from ()))))))))))
                                      (emeta
                                       ((loc
                                         ((begin_loc
                                           ((filename string) (line_num 1)
                                            (col_num 48) (included_from ())))
                                          (end_loc
                                           ((filename string) (line_num 1)
                                            (col_num 69) (included_from ()))))))))))
                                   (emeta
                                    ((loc
                                      ((begin_loc
                                        ((filename string) (line_num 1)
                                         (col_num 44) (included_from ())))
                                       (end_loc
                                        ((filename string) (line_num 1)
                                         (col_num 69) (included_from ()))))))))))
                                (emeta
                                 ((loc
                                   ((begin_loc
                                     ((filename string) (line_num 1) (col_num 34)
                                      (included_from ())))
                                    (end_loc
                                     ((filename string) (line_num 1) (col_num 69)
                                      (included_from ()))))))))))
                             (emeta
                              ((loc
                                ((begin_loc
                                  ((filename string) (line_num 1) (col_num 28)
                                   (included_from ())))
                                 (end_loc
                                  ((filename string) (line_num 1) (col_num 69)
                                   (included_from ()))))))))))
                          (emeta
                           ((loc
                             ((begin_loc
                               ((filename string) (line_num 1) (col_num 25)
                                (included_from ())))
                              (end_loc
                               ((filename string) (line_num 1) (col_num 69)
                                (included_from ()))))))))))
                       (emeta
                        ((loc
                          ((begin_loc
                            ((filename string) (line_num 1) (col_num 22)
                             (included_from ())))
                           (end_loc
                            ((filename string) (line_num 1) (col_num 69)
                             (included_from ()))))))))))
                    (emeta
                     ((loc
                       ((begin_loc
                         ((filename string) (line_num 1) (col_num 18)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 1) (col_num 69)
                          (included_from ())))))))))))
                (emeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 1) (col_num 15)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 1) (col_num 70)
                      (included_from ()))))))))))))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 1) (col_num 9) (included_from ())))
               (end_loc
                ((filename string) (line_num 1) (col_num 72) (included_from ())))))))))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse crazy truncation example" =
  print_ast_of_string
    "\n\
    \      model {\n\
    \        real T[1,1] = {{42.0}};\n\
    \        1 ~ normal(0, 1) T[1, T[1,1]];\n\
    \        print(T[1,1]);\n\
    \      }\n\
    \      " ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt
            (VarDecl
             (decl_type
              (Sized
               (SArray
                (SArray SReal
                 ((expr (IntNumeral 1))
                  (emeta
                   ((loc
                     ((begin_loc
                       ((filename string) (line_num 3) (col_num 17)
                        (included_from ())))
                      (end_loc
                       ((filename string) (line_num 3) (col_num 18)
                        (included_from ())))))))))
                ((expr (IntNumeral 1))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 3) (col_num 15)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 3) (col_num 16)
                       (included_from ())))))))))))
             (transformation Identity)
             (identifier
              ((name T)
               (id_loc
                ((begin_loc
                  ((filename string) (line_num 3) (col_num 13) (included_from ())))
                 (end_loc
                  ((filename string) (line_num 3) (col_num 14) (included_from ())))))))
             (initial_value
              (((expr
                 (ArrayExpr
                  (((expr
                     (ArrayExpr
                      (((expr (RealNumeral 42.0))
                        (emeta
                         ((loc
                           ((begin_loc
                             ((filename string) (line_num 3) (col_num 24)
                              (included_from ())))
                            (end_loc
                             ((filename string) (line_num 3) (col_num 28)
                              (included_from ())))))))))))
                    (emeta
                     ((loc
                       ((begin_loc
                         ((filename string) (line_num 3) (col_num 23)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 3) (col_num 29)
                          (included_from ())))))))))))
                (emeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 3) (col_num 22)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 3) (col_num 30)
                      (included_from ()))))))))))
             (is_global false)))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 3) (col_num 8) (included_from ())))
               (end_loc
                ((filename string) (line_num 3) (col_num 31) (included_from ()))))))))
          ((stmt
            (Tilde
             (arg
              ((expr (IntNumeral 1))
               (emeta
                ((loc
                  ((begin_loc
                    ((filename string) (line_num 4) (col_num 8) (included_from ())))
                   (end_loc
                    ((filename string) (line_num 4) (col_num 9) (included_from ())))))))))
             (distribution
              ((name normal)
               (id_loc
                ((begin_loc
                  ((filename string) (line_num 4) (col_num 12) (included_from ())))
                 (end_loc
                  ((filename string) (line_num 4) (col_num 18) (included_from ())))))))
             (args
              (((expr (IntNumeral 0))
                (emeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 4) (col_num 19)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 4) (col_num 20)
                      (included_from ()))))))))
               ((expr (IntNumeral 1))
                (emeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 4) (col_num 22)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 4) (col_num 23)
                      (included_from ()))))))))))
             (truncation
              (TruncateBetween
               ((expr (IntNumeral 1))
                (emeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 4) (col_num 27)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 4) (col_num 28)
                      (included_from ()))))))))
               ((expr
                 (Indexed
                  ((expr
                    (Variable
                     ((name T)
                      (id_loc
                       ((begin_loc
                         ((filename string) (line_num 4) (col_num 30)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 4) (col_num 31)
                          (included_from ()))))))))
                   (emeta
                    ((loc
                      ((begin_loc
                        ((filename string) (line_num 4) (col_num 30)
                         (included_from ())))
                       (end_loc
                        ((filename string) (line_num 4) (col_num 31)
                         (included_from ()))))))))
                  ((Single
                    ((expr (IntNumeral 1))
                     (emeta
                      ((loc
                        ((begin_loc
                          ((filename string) (line_num 4) (col_num 32)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 4) (col_num 33)
                           (included_from ())))))))))
                   (Single
                    ((expr (IntNumeral 1))
                     (emeta
                      ((loc
                        ((begin_loc
                          ((filename string) (line_num 4) (col_num 34)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 4) (col_num 35)
                           (included_from ()))))))))))))
                (emeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 4) (col_num 30)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 4) (col_num 36)
                      (included_from ()))))))))))))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 4) (col_num 8) (included_from ())))
               (end_loc
                ((filename string) (line_num 4) (col_num 38) (included_from ()))))))))
          ((stmt
            (Print
             ((PExpr
               ((expr
                 (Indexed
                  ((expr
                    (Variable
                     ((name T)
                      (id_loc
                       ((begin_loc
                         ((filename string) (line_num 5) (col_num 14)
                          (included_from ())))
                        (end_loc
                         ((filename string) (line_num 5) (col_num 15)
                          (included_from ()))))))))
                   (emeta
                    ((loc
                      ((begin_loc
                        ((filename string) (line_num 5) (col_num 14)
                         (included_from ())))
                       (end_loc
                        ((filename string) (line_num 5) (col_num 15)
                         (included_from ()))))))))
                  ((Single
                    ((expr (IntNumeral 1))
                     (emeta
                      ((loc
                        ((begin_loc
                          ((filename string) (line_num 5) (col_num 16)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 5) (col_num 17)
                           (included_from ())))))))))
                   (Single
                    ((expr (IntNumeral 1))
                     (emeta
                      ((loc
                        ((begin_loc
                          ((filename string) (line_num 5) (col_num 18)
                           (included_from ())))
                         (end_loc
                          ((filename string) (line_num 5) (col_num 19)
                           (included_from ()))))))))))))
                (emeta
                 ((loc
                   ((begin_loc
                     ((filename string) (line_num 5) (col_num 14)
                      (included_from ())))
                    (end_loc
                     ((filename string) (line_num 5) (col_num 20)
                      (included_from ()))))))))))))
           (smeta
            ((loc
              ((begin_loc
                ((filename string) (line_num 5) (col_num 8) (included_from ())))
               (end_loc
                ((filename string) (line_num 5) (col_num 22) (included_from ())))))))))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse nested loop" =
  print_ast_of_string
    "      model {\n\
    \              for (i in 1:2)\n\
    \                for (j in 3:4)\n\
    \                  print(\"Badger\");\n\
    \            }\n\
    \            " ;
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      ((((stmt
          (For
           (loop_variable
            ((name i)
             (id_loc
              ((begin_loc
                ((filename string) (line_num 2) (col_num 19) (included_from ())))
               (end_loc
                ((filename string) (line_num 2) (col_num 20) (included_from ())))))))
           (lower_bound
            ((expr (IntNumeral 1))
             (emeta
              ((loc
                ((begin_loc
                  ((filename string) (line_num 2) (col_num 24)
                   (included_from ())))
                 (end_loc
                  ((filename string) (line_num 2) (col_num 25)
                   (included_from ())))))))))
           (upper_bound
            ((expr (IntNumeral 2))
             (emeta
              ((loc
                ((begin_loc
                  ((filename string) (line_num 2) (col_num 26)
                   (included_from ())))
                 (end_loc
                  ((filename string) (line_num 2) (col_num 27)
                   (included_from ())))))))))
           (loop_body
            ((stmt
              (For
               (loop_variable
                ((name j)
                 (id_loc
                  ((begin_loc
                    ((filename string) (line_num 3) (col_num 21)
                     (included_from ())))
                   (end_loc
                    ((filename string) (line_num 3) (col_num 22)
                     (included_from ())))))))
               (lower_bound
                ((expr (IntNumeral 3))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 3) (col_num 26)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 3) (col_num 27)
                       (included_from ())))))))))
               (upper_bound
                ((expr (IntNumeral 4))
                 (emeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 3) (col_num 28)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 3) (col_num 29)
                       (included_from ())))))))))
               (loop_body
                ((stmt (Print ((PString "\"Badger\""))))
                 (smeta
                  ((loc
                    ((begin_loc
                      ((filename string) (line_num 4) (col_num 18)
                       (included_from ())))
                     (end_loc
                      ((filename string) (line_num 4) (col_num 34)
                       (included_from ())))))))))))
             (smeta
              ((loc
                ((begin_loc
                  ((filename string) (line_num 3) (col_num 16)
                   (included_from ())))
                 (end_loc
                  ((filename string) (line_num 4) (col_num 34)
                   (included_from ())))))))))))
         (smeta
          ((loc
            ((begin_loc
              ((filename string) (line_num 2) (col_num 14) (included_from ())))
             (end_loc
              ((filename string) (line_num 4) (col_num 34) (included_from ())))))))))))
     (generatedquantitiesblock ())) |}]
