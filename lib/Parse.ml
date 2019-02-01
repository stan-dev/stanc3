(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Core_kernel
open Errors

let parse parse_fun lexbuf =
  (* see the Menhir manual for the description of
     error messages support *)
  let open MenhirLib.General in
  let module Interp = Parser.MenhirInterpreter in
  let _ = Stack.push Preprocessor.include_stack lexbuf in
  let input _ =
    (Interp.lexer_lexbuf_to_supplier Lexer.token
       (Stack.top_exn Preprocessor.include_stack))
      ()
  in
  let success prog = prog in
  let failure error_state =
    let env =
      match[@warning "-4"] error_state with
      | Interp.HandlingError env -> env
      | _ -> assert false
    in
    match Interp.stack env with
    | (lazy Nil) ->
        let message =
          "Expected \"functions {\" or \"data {\" or \"transformed data {\" \
           or \"parameters {\" or \"transformed parameters {\" or \"model {\" \
           or \"generated quantities {\".\n"
        in
        raise
          (SyntaxError
             (Parsing
                ( message
                , Errors.loc_span_of_pos
                    (Lexing.lexeme_start_p
                       (Stack.top_exn Preprocessor.include_stack))
                    (Lexing.lexeme_end_p
                       (Stack.top_exn Preprocessor.include_stack)) )))
    | (lazy (Cons (Interp.Element (state, _, start_pos, end_pos), _))) ->
        let message =
          try
            Parsing_errors.message (Interp.number state)
            ^
            if !Debugging.grammar_logging then
              "(Parse error state " ^ string_of_int (Interp.number state) ^ ")"
            else ""
          with Not_found_s _ ->
            if !Debugging.grammar_logging then
              "(Parse error state " ^ string_of_int (Interp.number state) ^ ")"
            else ""
        in
        raise
          (SyntaxError
             (Parsing (message, Errors.loc_span_of_pos start_pos end_pos)))
  in
  Interp.loop_handle success failure input (parse_fun lexbuf.Lexing.lex_curr_p)

let parse_string parse_fun str =
  let lexbuf =
    let open Lexing in
    let lexbuf = from_string str in
    lexbuf.lex_start_p
    <- {pos_fname= "string"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
    lexbuf
  in
  parse parse_fun lexbuf

let parse_file parse_fun path =
  let chan = In_channel.create path in
  let lexbuf =
    let open Lexing in
    let lexbuf = from_channel chan in
    lexbuf.lex_start_p
    <- {pos_fname= path; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
    lexbuf
  in
  parse parse_fun lexbuf

(* TESTS *)
let%expect_test "parse conditional" =
  let ast =
    parse_string Parser.Incremental.program
      "model { if (1 < 2) { print(\"hi\");}}"
  in
  print_s [%sexp (ast : Ast.untyped_program)] ;
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      ((((stmt_untyped
          (IfThenElse
           ((expr_untyped
             (BinOp ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>))
              Less ((expr_untyped (IntNumeral 2)) (expr_untyped_loc <opaque>))))
            (expr_untyped_loc <opaque>))
           ((stmt_untyped
             (Block
              (((stmt_untyped (Print ((PString "\"hi\""))))
                (stmt_untyped_loc <opaque>)))))
            (stmt_untyped_loc <opaque>))
           ()))
         (stmt_untyped_loc <opaque>)))))
     (generatedquantitiesblock ())) |}]

let%expect_test "parse dangling else problem" =
  let ast =
    parse_string Parser.Incremental.program
      "model { if (1 < 2) print(\"I'm sorry\"); if (2 < 3) print(\", Dave, \
       \"); else print(\"I'm afraid I can't do that.\");}"
  in
  print_s [%sexp (ast : Ast.untyped_program)] ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt_untyped
            (IfThenElse
             ((expr_untyped
               (BinOp ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>))
                Less ((expr_untyped (IntNumeral 2)) (expr_untyped_loc <opaque>))))
              (expr_untyped_loc <opaque>))
             ((stmt_untyped (Print ((PString "\"I'm sorry\""))))
              (stmt_untyped_loc <opaque>))
             ()))
           (stmt_untyped_loc <opaque>))
          ((stmt_untyped
            (IfThenElse
             ((expr_untyped
               (BinOp ((expr_untyped (IntNumeral 2)) (expr_untyped_loc <opaque>))
                Less ((expr_untyped (IntNumeral 3)) (expr_untyped_loc <opaque>))))
              (expr_untyped_loc <opaque>))
             ((stmt_untyped (Print ((PString "\", Dave, \""))))
              (stmt_untyped_loc <opaque>))
             (((stmt_untyped (Print ((PString "\"I'm afraid I can't do that.\""))))
               (stmt_untyped_loc <opaque>)))))
           (stmt_untyped_loc <opaque>)))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse minus unary" =
  let ast =
    parse_string Parser.Incremental.program "model { real x; x = -x;}"
  in
  print_s [%sexp (ast : Ast.untyped_program)] ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt_untyped
            (VarDecl (sizedtype SReal) (transformation Identity)
             (identifier ((name x) (id_loc <opaque>))) (initial_value ())
             (is_global false)))
           (stmt_untyped_loc <opaque>))
          ((stmt_untyped
            (Assignment (assign_identifier ((name x) (id_loc <opaque>)))
             (assign_indices ()) (assign_op Assign)
             (assign_rhs
              ((expr_untyped
                (PrefixOp Minus
                 ((expr_untyped (Variable ((name x) (id_loc <opaque>))))
                  (expr_untyped_loc <opaque>))))
               (expr_untyped_loc <opaque>)))))
           (stmt_untyped_loc <opaque>)))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse unary over binary" =
  let ast =
    parse_string Parser.Incremental.program "model { real x = x - - x - - x; }"
  in
  print_s [%sexp (ast : Ast.untyped_program)] ;
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      ((((stmt_untyped
          (VarDecl (sizedtype SReal) (transformation Identity)
           (identifier ((name x) (id_loc <opaque>)))
           (initial_value
            (((expr_untyped
               (BinOp
                ((expr_untyped
                  (BinOp
                   ((expr_untyped (Variable ((name x) (id_loc <opaque>))))
                    (expr_untyped_loc <opaque>))
                   Minus
                   ((expr_untyped
                     (PrefixOp Minus
                      ((expr_untyped (Variable ((name x) (id_loc <opaque>))))
                       (expr_untyped_loc <opaque>))))
                    (expr_untyped_loc <opaque>))))
                 (expr_untyped_loc <opaque>))
                Minus
                ((expr_untyped
                  (PrefixOp Minus
                   ((expr_untyped (Variable ((name x) (id_loc <opaque>))))
                    (expr_untyped_loc <opaque>))))
                 (expr_untyped_loc <opaque>))))
              (expr_untyped_loc <opaque>))))
           (is_global false)))
         (stmt_untyped_loc <opaque>)))))
     (generatedquantitiesblock ())) |}]

let%expect_test "parse indices, two different colons" =
  let ast =
    parse_string Parser.Incremental.program
      "model { matrix[5, 5] x; print(x[2 - 3 ? 3 : 4 : 2]); }"
  in
  print_s [%sexp (ast : Ast.untyped_program)] ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt_untyped
            (VarDecl
             (sizedtype
              (SMatrix ((expr_untyped (IntNumeral 5)) (expr_untyped_loc <opaque>))
               ((expr_untyped (IntNumeral 5)) (expr_untyped_loc <opaque>))))
             (transformation Identity) (identifier ((name x) (id_loc <opaque>)))
             (initial_value ()) (is_global false)))
           (stmt_untyped_loc <opaque>))
          ((stmt_untyped
            (Print
             ((PExpr
               ((expr_untyped
                 (Indexed
                  ((expr_untyped (Variable ((name x) (id_loc <opaque>))))
                   (expr_untyped_loc <opaque>))
                  ((Between
                    ((expr_untyped
                      (TernaryIf
                       ((expr_untyped
                         (BinOp
                          ((expr_untyped (IntNumeral 2))
                           (expr_untyped_loc <opaque>))
                          Minus
                          ((expr_untyped (IntNumeral 3))
                           (expr_untyped_loc <opaque>))))
                        (expr_untyped_loc <opaque>))
                       ((expr_untyped (IntNumeral 3)) (expr_untyped_loc <opaque>))
                       ((expr_untyped (IntNumeral 4)) (expr_untyped_loc <opaque>))))
                     (expr_untyped_loc <opaque>))
                    ((expr_untyped (IntNumeral 2)) (expr_untyped_loc <opaque>))))))
                (expr_untyped_loc <opaque>))))))
           (stmt_untyped_loc <opaque>)))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse operator precedence" =
  let ast =
    parse_string Parser.Incremental.program
      "model {  \
       print({a,b?c:d||e&&f==g!=h<=i<j>=k>l+m-n*o/p%q.*s./t\\r^u[v]'}); }"
  in
  print_s [%sexp (ast : Ast.untyped_program)] ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt_untyped
            (Print
             ((PExpr
               ((expr_untyped
                 (ArrayExpr
                  (((expr_untyped (Variable ((name a) (id_loc <opaque>))))
                    (expr_untyped_loc <opaque>))
                   ((expr_untyped
                     (TernaryIf
                      ((expr_untyped (Variable ((name b) (id_loc <opaque>))))
                       (expr_untyped_loc <opaque>))
                      ((expr_untyped (Variable ((name c) (id_loc <opaque>))))
                       (expr_untyped_loc <opaque>))
                      ((expr_untyped
                        (BinOp
                         ((expr_untyped (Variable ((name d) (id_loc <opaque>))))
                          (expr_untyped_loc <opaque>))
                         Or
                         ((expr_untyped
                           (BinOp
                            ((expr_untyped (Variable ((name e) (id_loc <opaque>))))
                             (expr_untyped_loc <opaque>))
                            And
                            ((expr_untyped
                              (BinOp
                               ((expr_untyped
                                 (BinOp
                                  ((expr_untyped
                                    (Variable ((name f) (id_loc <opaque>))))
                                   (expr_untyped_loc <opaque>))
                                  Equals
                                  ((expr_untyped
                                    (Variable ((name g) (id_loc <opaque>))))
                                   (expr_untyped_loc <opaque>))))
                                (expr_untyped_loc <opaque>))
                               NEquals
                               ((expr_untyped
                                 (BinOp
                                  ((expr_untyped
                                    (BinOp
                                     ((expr_untyped
                                       (BinOp
                                        ((expr_untyped
                                          (BinOp
                                           ((expr_untyped
                                             (Variable
                                              ((name h) (id_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))
                                           Leq
                                           ((expr_untyped
                                             (Variable
                                              ((name i) (id_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))
                                        Less
                                        ((expr_untyped
                                          (Variable ((name j) (id_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))))
                                      (expr_untyped_loc <opaque>))
                                     Geq
                                     ((expr_untyped
                                       (Variable ((name k) (id_loc <opaque>))))
                                      (expr_untyped_loc <opaque>))))
                                   (expr_untyped_loc <opaque>))
                                  Greater
                                  ((expr_untyped
                                    (BinOp
                                     ((expr_untyped
                                       (BinOp
                                        ((expr_untyped
                                          (Variable ((name l) (id_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))
                                        Plus
                                        ((expr_untyped
                                          (Variable ((name m) (id_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))))
                                      (expr_untyped_loc <opaque>))
                                     Minus
                                     ((expr_untyped
                                       (BinOp
                                        ((expr_untyped
                                          (BinOp
                                           ((expr_untyped
                                             (BinOp
                                              ((expr_untyped
                                                (BinOp
                                                 ((expr_untyped
                                                   (BinOp
                                                    ((expr_untyped
                                                      (Variable
                                                       ((name n) (id_loc <opaque>))))
                                                     (expr_untyped_loc <opaque>))
                                                    Times
                                                    ((expr_untyped
                                                      (Variable
                                                       ((name o) (id_loc <opaque>))))
                                                     (expr_untyped_loc <opaque>))))
                                                  (expr_untyped_loc <opaque>))
                                                 Divide
                                                 ((expr_untyped
                                                   (Variable
                                                    ((name p) (id_loc <opaque>))))
                                                  (expr_untyped_loc <opaque>))))
                                               (expr_untyped_loc <opaque>))
                                              Modulo
                                              ((expr_untyped
                                                (Variable
                                                 ((name q) (id_loc <opaque>))))
                                               (expr_untyped_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))
                                           EltTimes
                                           ((expr_untyped
                                             (Variable
                                              ((name s) (id_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))
                                        EltDivide
                                        ((expr_untyped
                                          (BinOp
                                           ((expr_untyped
                                             (Variable
                                              ((name t) (id_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))
                                           LDivide
                                           ((expr_untyped
                                             (BinOp
                                              ((expr_untyped
                                                (Variable
                                                 ((name r) (id_loc <opaque>))))
                                               (expr_untyped_loc <opaque>))
                                              Exp
                                              ((expr_untyped
                                                (PostfixOp
                                                 ((expr_untyped
                                                   (Indexed
                                                    ((expr_untyped
                                                      (Variable
                                                       ((name u) (id_loc <opaque>))))
                                                     (expr_untyped_loc <opaque>))
                                                    ((Single
                                                      ((expr_untyped
                                                        (Variable
                                                         ((name v)
                                                          (id_loc <opaque>))))
                                                       (expr_untyped_loc <opaque>))))))
                                                  (expr_untyped_loc <opaque>))
                                                 Transpose))
                                               (expr_untyped_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))))
                                      (expr_untyped_loc <opaque>))))
                                   (expr_untyped_loc <opaque>))))
                                (expr_untyped_loc <opaque>))))
                             (expr_untyped_loc <opaque>))))
                          (expr_untyped_loc <opaque>))))
                       (expr_untyped_loc <opaque>))))
                    (expr_untyped_loc <opaque>)))))
                (expr_untyped_loc <opaque>))))))
           (stmt_untyped_loc <opaque>)))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse crazy truncation example" =
  let ast =
    parse_string Parser.Incremental.program
      "\n\
      \      model {\n\
      \        real T[1,1] = {{42.0}};\n\
      \        1 ~ normal(0, 1) T[1, T[1,1]];\n\
      \        print(T[1,1]);\n\
      \      }\n\
      \      "
  in
  print_s [%sexp (ast : Ast.untyped_program)] ;
  [%expect
    {|
      ((functionblock ()) (datablock ()) (transformeddatablock ())
       (parametersblock ()) (transformedparametersblock ())
       (modelblock
        ((((stmt_untyped
            (VarDecl
             (sizedtype
              (SArray
               (SArray SReal
                ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>)))
               ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>))))
             (transformation Identity) (identifier ((name T) (id_loc <opaque>)))
             (initial_value
              (((expr_untyped
                 (ArrayExpr
                  (((expr_untyped
                     (ArrayExpr
                      (((expr_untyped (RealNumeral 42.0))
                        (expr_untyped_loc <opaque>)))))
                    (expr_untyped_loc <opaque>)))))
                (expr_untyped_loc <opaque>))))
             (is_global false)))
           (stmt_untyped_loc <opaque>))
          ((stmt_untyped
            (Tilde
             (arg ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>)))
             (distribution ((name normal) (id_loc <opaque>)))
             (args
              (((expr_untyped (IntNumeral 0)) (expr_untyped_loc <opaque>))
               ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>))))
             (truncation
              (TruncateBetween
               ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>))
               ((expr_untyped
                 (Indexed
                  ((expr_untyped (Variable ((name T) (id_loc <opaque>))))
                   (expr_untyped_loc <opaque>))
                  ((Single
                    ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>)))
                   (Single
                    ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>))))))
                (expr_untyped_loc <opaque>))))))
           (stmt_untyped_loc <opaque>))
          ((stmt_untyped
            (Print
             ((PExpr
               ((expr_untyped
                 (Indexed
                  ((expr_untyped (Variable ((name T) (id_loc <opaque>))))
                   (expr_untyped_loc <opaque>))
                  ((Single
                    ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>)))
                   (Single
                    ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>))))))
                (expr_untyped_loc <opaque>))))))
           (stmt_untyped_loc <opaque>)))))
       (generatedquantitiesblock ())) |}]

let%expect_test "parse nested loop" =
  let ast =
    parse_string Parser.Incremental.program
      "      model {\n\
      \              for (i in 1:2)\n\
      \                for (j in 3:4)\n\
      \                  print(\"Badger\");\n\
      \            }\n\
      \            "
  in
  print_s [%sexp (ast : Ast.untyped_program)] ;
  [%expect
    {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      ((((stmt_untyped
          (For (loop_variable ((name i) (id_loc <opaque>)))
           (lower_bound
            ((expr_untyped (IntNumeral 1)) (expr_untyped_loc <opaque>)))
           (upper_bound
            ((expr_untyped (IntNumeral 2)) (expr_untyped_loc <opaque>)))
           (loop_body
            ((stmt_untyped
              (For (loop_variable ((name j) (id_loc <opaque>)))
               (lower_bound
                ((expr_untyped (IntNumeral 3)) (expr_untyped_loc <opaque>)))
               (upper_bound
                ((expr_untyped (IntNumeral 4)) (expr_untyped_loc <opaque>)))
               (loop_body
                ((stmt_untyped (Print ((PString "\"Badger\""))))
                 (stmt_untyped_loc <opaque>)))))
             (stmt_untyped_loc <opaque>)))))
         (stmt_untyped_loc <opaque>)))))
     (generatedquantitiesblock ())) |}]
