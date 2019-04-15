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
      ((((stmt
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
         (smeta ((loc <opaque>)))))))
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
        ((((stmt
            (IfThenElse
             ((expr
               (BinOp ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))) Less
                ((expr (IntNumeral 2)) (emeta ((loc <opaque>))))))
              (emeta ((loc <opaque>))))
             ((stmt (Print ((PString "\"I'm sorry\"")))) (smeta ((loc <opaque>))))
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
           (smeta ((loc <opaque>)))))))
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
        ((((stmt
            (VarDecl (sizedtype SReal) (transformation Identity)
             (identifier ((name x) (id_loc <opaque>))) (initial_value ())
             (is_global false)))
           (smeta ((loc <opaque>))))
          ((stmt
            (Assignment (assign_identifier ((name x) (id_loc <opaque>)))
             (assign_indices ()) (assign_op Assign)
             (assign_rhs
              ((expr
                (PrefixOp PMinus
                 ((expr (Variable ((name x) (id_loc <opaque>))))
                  (emeta ((loc <opaque>))))))
               (emeta ((loc <opaque>)))))))
           (smeta ((loc <opaque>)))))))
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
      ((((stmt
          (VarDecl (sizedtype SReal) (transformation Identity)
           (identifier ((name x) (id_loc <opaque>)))
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
              (emeta ((loc <opaque>))))))
           (is_global false)))
         (smeta ((loc <opaque>)))))))
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
        ((((stmt
            (VarDecl
             (sizedtype
              (SMatrix ((expr (IntNumeral 5)) (emeta ((loc <opaque>))))
               ((expr (IntNumeral 5)) (emeta ((loc <opaque>))))))
             (transformation Identity) (identifier ((name x) (id_loc <opaque>)))
             (initial_value ()) (is_global false)))
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
           (smeta ((loc <opaque>)))))))
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
        ((((stmt
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
                                                       ((name n) (id_loc <opaque>))))
                                                     (emeta ((loc <opaque>))))
                                                    Times
                                                    ((expr
                                                      (Variable
                                                       ((name o) (id_loc <opaque>))))
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
                                                       ((name u) (id_loc <opaque>))))
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
           (smeta ((loc <opaque>)))))))
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
        ((((stmt
            (VarDecl
             (sizedtype
              (SArray
               (SArray SReal ((expr (IntNumeral 1)) (emeta ((loc <opaque>)))))
               ((expr (IntNumeral 1)) (emeta ((loc <opaque>))))))
             (transformation Identity) (identifier ((name T) (id_loc <opaque>)))
             (initial_value
              (((expr
                 (ArrayExpr
                  (((expr
                     (ArrayExpr
                      (((expr (RealNumeral 42.0)) (emeta ((loc <opaque>)))))))
                    (emeta ((loc <opaque>)))))))
                (emeta ((loc <opaque>))))))
             (is_global false)))
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
           (smeta ((loc <opaque>)))))))
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
      ((((stmt
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
         (smeta ((loc <opaque>)))))))
     (generatedquantitiesblock ())) |}]
