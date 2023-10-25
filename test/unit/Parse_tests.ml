open Core
open Frontend

let print_ast_of_string s =
  let ast =
    Test_utils.untyped_ast_of_string s
    |> Result.map_error ~f:Errors.to_string
    |> Result.ok_or_failwith in
  print_s [%sexp (ast : Ast.untyped_program)]

(* TESTS *)
let%expect_test "parse conditional" =
  print_ast_of_string "model { if (1 < 2) { print(\"hi\");}}";
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      (((stmts
         (((stmt
            (IfThenElse
             ((expr
               (BinOp ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))) Less
                ((expr (IntNumeral 2)) (emeta ((loc <opaque>))))))
              (emeta ((loc <opaque>))))
             ((stmt
               (Block
                (((stmt (Print ((PString "\"hi\"")))) (smeta ((loc <opaque>)))))))
              (smeta ((loc <opaque>))))
             ()))
           (smeta ((loc <opaque>))))))
        (xloc
         ((begin_loc
           ((filename string) (line_num 1) (col_num 0) (included_from ())))
          (end_loc
           ((filename string) (line_num 1) (col_num 35) (included_from ()))))))))
     (generatedquantitiesblock ()) (comments <opaque>)) |}]

let%expect_test "parse dangling else problem" =
  print_ast_of_string
    "model { if (1 < 2) print(\"I'm sorry\"); if (2 < 3) print(\", Dave, \"); \
     else print(\"I'm afraid I can't do that.\");}";
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        (((stmts
           (((stmt
              (IfThenElse
               ((expr
                 (BinOp ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))) Less
                  ((expr (IntNumeral 2)) (emeta ((loc <opaque>))))))
                (emeta ((loc <opaque>))))
               ((stmt (Print ((PString "\"I'm sorry\""))))
                (smeta ((loc <opaque>))))
               ()))
             (smeta ((loc <opaque>))))
            ((stmt
              (IfThenElse
               ((expr
                 (BinOp ((expr (IntNumeral 2)) (emeta ((loc <opaque>)))) Less
                  ((expr (IntNumeral 3)) (emeta ((loc <opaque>))))))
                (emeta ((loc <opaque>))))
               ((stmt (Print ((PString "\", Dave, \"")))) (smeta ((loc <opaque>))))
               (((stmt (Print ((PString "\"I'm afraid I can't do that.\""))))
                 (smeta ((loc <opaque>)))))))
             (smeta ((loc <opaque>))))))
          (xloc
           ((begin_loc
             ((filename string) (line_num 1) (col_num 0) (included_from ())))
            (end_loc
             ((filename string) (line_num 1) (col_num 112) (included_from ()))))))))
       (generatedquantitiesblock ()) (comments <opaque>)) |}]

let%expect_test "parse minus unary" =
  print_ast_of_string "model { real x; x = -x;}";
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        (((stmts
           (((stmt
              (VarDecl (decl_type SReal) (transformation Identity)
               (is_global false)
               (variables
                (((identifier ((name x) (id_loc <opaque>))) (initial_value ()))))))
             (smeta ((loc <opaque>))))
            ((stmt
              (Assignment
               (assign_lhs
                (LValue
                 ((lval (LVariable ((name x) (id_loc <opaque>))))
                  (lmeta ((loc <opaque>))))))
               (assign_op Assign)
               (assign_rhs
                ((expr
                  (PrefixOp PMinus
                   ((expr (Variable ((name x) (id_loc <opaque>))))
                    (emeta ((loc <opaque>))))))
                 (emeta ((loc <opaque>)))))))
             (smeta ((loc <opaque>))))))
          (xloc
           ((begin_loc
             ((filename string) (line_num 1) (col_num 0) (included_from ())))
            (end_loc
             ((filename string) (line_num 1) (col_num 24) (included_from ()))))))))
       (generatedquantitiesblock ()) (comments <opaque>)) |}]

let%expect_test "parse unary over binary" =
  print_ast_of_string "model { real x = x - - x - - x; }";
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      (((stmts
         (((stmt
            (VarDecl (decl_type SReal) (transformation Identity)
             (is_global false)
             (variables
              (((identifier ((name x) (id_loc <opaque>)))
                (initial_value
                 (((expr
                    (BinOp
                     ((expr
                       (BinOp
                        ((expr (Variable ((name x) (id_loc <opaque>))))
                         (emeta ((loc <opaque>))))
                        Minus
                        ((expr
                          (PrefixOp PMinus
                           ((expr (Variable ((name x) (id_loc <opaque>))))
                            (emeta ((loc <opaque>))))))
                         (emeta ((loc <opaque>))))))
                      (emeta ((loc <opaque>))))
                     Minus
                     ((expr
                       (PrefixOp PMinus
                        ((expr (Variable ((name x) (id_loc <opaque>))))
                         (emeta ((loc <opaque>))))))
                      (emeta ((loc <opaque>))))))
                   (emeta ((loc <opaque>)))))))))))
           (smeta ((loc <opaque>))))))
        (xloc
         ((begin_loc
           ((filename string) (line_num 1) (col_num 0) (included_from ())))
          (end_loc
           ((filename string) (line_num 1) (col_num 33) (included_from ()))))))))
     (generatedquantitiesblock ()) (comments <opaque>)) |}]

let%expect_test "parse indices, two different colons" =
  print_ast_of_string "model { matrix[5, 5] x; print(x[2 - 3 ? 3 : 4 : 2]); }";
  [%expect
    {|
((functionblock ()) (datablock ()) (transformeddatablock ())
 (parametersblock ()) (transformedparametersblock ())
 (modelblock
  (((stmts
     (((stmt
        (VarDecl
         (decl_type
          (SMatrix AoS ((expr (IntNumeral 5)) (emeta ((loc <opaque>))))
           ((expr (IntNumeral 5)) (emeta ((loc <opaque>))))))
         (transformation Identity) (is_global false)
         (variables
          (((identifier ((name x) (id_loc <opaque>))) (initial_value ()))))))
       (smeta ((loc <opaque>))))
      ((stmt
        (Print
         ((PExpr
           ((expr
             (Indexed
              ((expr (Variable ((name x) (id_loc <opaque>))))
               (emeta ((loc <opaque>))))
              ((Between
                ((expr
                  (TernaryIf
                   ((expr
                     (BinOp ((expr (IntNumeral 2)) (emeta ((loc <opaque>))))
                      Minus ((expr (IntNumeral 3)) (emeta ((loc <opaque>))))))
                    (emeta ((loc <opaque>))))
                   ((expr (IntNumeral 3)) (emeta ((loc <opaque>))))
                   ((expr (IntNumeral 4)) (emeta ((loc <opaque>))))))
                 (emeta ((loc <opaque>))))
                ((expr (IntNumeral 2)) (emeta ((loc <opaque>))))))))
            (emeta ((loc <opaque>))))))))
       (smeta ((loc <opaque>))))))
    (xloc
     ((begin_loc
       ((filename string) (line_num 1) (col_num 0) (included_from ())))
      (end_loc
       ((filename string) (line_num 1) (col_num 54) (included_from ()))))))))
 (generatedquantitiesblock ()) (comments <opaque>)) |}]

let%expect_test "parse operator precedence" =
  print_ast_of_string
    "model {  print({a,b?c:d||e&&f==g!=h<=i<j>=k>l+m-n*o/p%q.*s./t\\r^u[v]'}); \
     }";
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        (((stmts
           (((stmt
              (Print
               ((PExpr
                 ((expr
                   (ArrayExpr
                    (((expr (Variable ((name a) (id_loc <opaque>))))
                      (emeta ((loc <opaque>))))
                     ((expr
                       (TernaryIf
                        ((expr (Variable ((name b) (id_loc <opaque>))))
                         (emeta ((loc <opaque>))))
                        ((expr (Variable ((name c) (id_loc <opaque>))))
                         (emeta ((loc <opaque>))))
                        ((expr
                          (BinOp
                           ((expr (Variable ((name d) (id_loc <opaque>))))
                            (emeta ((loc <opaque>))))
                           Or
                           ((expr
                             (BinOp
                              ((expr (Variable ((name e) (id_loc <opaque>))))
                               (emeta ((loc <opaque>))))
                              And
                              ((expr
                                (BinOp
                                 ((expr
                                   (BinOp
                                    ((expr (Variable ((name f) (id_loc <opaque>))))
                                     (emeta ((loc <opaque>))))
                                    Equals
                                    ((expr (Variable ((name g) (id_loc <opaque>))))
                                     (emeta ((loc <opaque>))))))
                                  (emeta ((loc <opaque>))))
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
                                                ((name h) (id_loc <opaque>))))
                                              (emeta ((loc <opaque>))))
                                             Leq
                                             ((expr
                                               (Variable
                                                ((name i) (id_loc <opaque>))))
                                              (emeta ((loc <opaque>))))))
                                           (emeta ((loc <opaque>))))
                                          Less
                                          ((expr
                                            (Variable ((name j) (id_loc <opaque>))))
                                           (emeta ((loc <opaque>))))))
                                        (emeta ((loc <opaque>))))
                                       Geq
                                       ((expr
                                         (Variable ((name k) (id_loc <opaque>))))
                                        (emeta ((loc <opaque>))))))
                                     (emeta ((loc <opaque>))))
                                    Greater
                                    ((expr
                                      (BinOp
                                       ((expr
                                         (BinOp
                                          ((expr
                                            (Variable ((name l) (id_loc <opaque>))))
                                           (emeta ((loc <opaque>))))
                                          Plus
                                          ((expr
                                            (Variable ((name m) (id_loc <opaque>))))
                                           (emeta ((loc <opaque>))))))
                                        (emeta ((loc <opaque>))))
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
                                                          (id_loc <opaque>))))
                                                       (emeta ((loc <opaque>))))
                                                      Times
                                                      ((expr
                                                        (Variable
                                                         ((name o)
                                                          (id_loc <opaque>))))
                                                       (emeta ((loc <opaque>))))))
                                                    (emeta ((loc <opaque>))))
                                                   Divide
                                                   ((expr
                                                     (Variable
                                                      ((name p) (id_loc <opaque>))))
                                                    (emeta ((loc <opaque>))))))
                                                 (emeta ((loc <opaque>))))
                                                Modulo
                                                ((expr
                                                  (Variable
                                                   ((name q) (id_loc <opaque>))))
                                                 (emeta ((loc <opaque>))))))
                                              (emeta ((loc <opaque>))))
                                             EltTimes
                                             ((expr
                                               (Variable
                                                ((name s) (id_loc <opaque>))))
                                              (emeta ((loc <opaque>))))))
                                           (emeta ((loc <opaque>))))
                                          EltDivide
                                          ((expr
                                            (BinOp
                                             ((expr
                                               (Variable
                                                ((name t) (id_loc <opaque>))))
                                              (emeta ((loc <opaque>))))
                                             LDivide
                                             ((expr
                                               (BinOp
                                                ((expr
                                                  (Variable
                                                   ((name r) (id_loc <opaque>))))
                                                 (emeta ((loc <opaque>))))
                                                Pow
                                                ((expr
                                                  (PostfixOp
                                                   ((expr
                                                     (Indexed
                                                      ((expr
                                                        (Variable
                                                         ((name u)
                                                          (id_loc <opaque>))))
                                                       (emeta ((loc <opaque>))))
                                                      ((Single
                                                        ((expr
                                                          (Variable
                                                           ((name v)
                                                            (id_loc <opaque>))))
                                                         (emeta ((loc <opaque>))))))))
                                                    (emeta ((loc <opaque>))))
                                                   Transpose))
                                                 (emeta ((loc <opaque>))))))
                                              (emeta ((loc <opaque>))))))
                                           (emeta ((loc <opaque>))))))
                                        (emeta ((loc <opaque>))))))
                                     (emeta ((loc <opaque>))))))
                                  (emeta ((loc <opaque>))))))
                               (emeta ((loc <opaque>))))))
                            (emeta ((loc <opaque>))))))
                         (emeta ((loc <opaque>))))))
                      (emeta ((loc <opaque>)))))))
                  (emeta ((loc <opaque>))))))))
             (smeta ((loc <opaque>))))))
          (xloc
           ((begin_loc
             ((filename string) (line_num 1) (col_num 0) (included_from ())))
            (end_loc
             ((filename string) (line_num 1) (col_num 74) (included_from ()))))))))
       (generatedquantitiesblock ()) (comments <opaque>)) |}]

let%expect_test "parse crazy truncation example" =
  print_ast_of_string
    "\n\
    \      model {\n\
    \        array[1,1] real T = {{42.0}};\n\
    \        1 ~ normal(0, 1) T[1, T[1,1]];\n\
    \        print(T[1,1]);\n\
    \      }\n\
    \      ";
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        (((stmts
           (((stmt
              (VarDecl
               (decl_type
                (SArray
                 (SArray SReal ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))))
                 ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))
               (transformation Identity) (is_global false)
               (variables
                (((identifier ((name T) (id_loc <opaque>)))
                  (initial_value
                   (((expr
                      (ArrayExpr
                       (((expr
                          (ArrayExpr
                           (((expr (RealNumeral 42.0)) (emeta ((loc <opaque>)))))))
                         (emeta ((loc <opaque>)))))))
                     (emeta ((loc <opaque>)))))))))))
             (smeta ((loc <opaque>))))
            ((stmt
              (Tilde (arg ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))))
               (distribution ((name normal) (id_loc <opaque>)))
               (args
                (((expr (IntNumeral 0)) (emeta ((loc <opaque>))))
                 ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))
               (truncation
                (TruncateBetween ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))
                 ((expr
                   (Indexed
                    ((expr (Variable ((name T) (id_loc <opaque>))))
                     (emeta ((loc <opaque>))))
                    ((Single ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))))
                     (Single ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))))
                  (emeta ((loc <opaque>))))))))
             (smeta ((loc <opaque>))))
            ((stmt
              (Print
               ((PExpr
                 ((expr
                   (Indexed
                    ((expr (Variable ((name T) (id_loc <opaque>))))
                     (emeta ((loc <opaque>))))
                    ((Single ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))))
                     (Single ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))))
                  (emeta ((loc <opaque>))))))))
             (smeta ((loc <opaque>))))))
          (xloc
           ((begin_loc
             ((filename string) (line_num 2) (col_num 6) (included_from ())))
            (end_loc
             ((filename string) (line_num 6) (col_num 7) (included_from ()))))))))
       (generatedquantitiesblock ()) (comments <opaque>)) |}]

let%expect_test "parse nested loop" =
  print_ast_of_string
    "      model {\n\
    \              for (i in 1:2)\n\
    \                for (j in 3:4)\n\
    \                  print(\"Badger\");\n\
    \            }\n\
    \            ";
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      (((stmts
         (((stmt
            (For (loop_variable ((name i) (id_loc <opaque>)))
             (lower_bound ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))))
             (upper_bound ((expr (IntNumeral 2)) (emeta ((loc <opaque>)))))
             (loop_body
              ((stmt
                (For (loop_variable ((name j) (id_loc <opaque>)))
                 (lower_bound ((expr (IntNumeral 3)) (emeta ((loc <opaque>)))))
                 (upper_bound ((expr (IntNumeral 4)) (emeta ((loc <opaque>)))))
                 (loop_body
                  ((stmt (Print ((PString "\"Badger\""))))
                   (smeta ((loc <opaque>)))))))
               (smeta ((loc <opaque>)))))))
           (smeta ((loc <opaque>))))))
        (xloc
         ((begin_loc
           ((filename string) (line_num 1) (col_num 6) (included_from ())))
          (end_loc
           ((filename string) (line_num 5) (col_num 13) (included_from ()))))))))
     (generatedquantitiesblock ()) (comments <opaque>)) |}]
