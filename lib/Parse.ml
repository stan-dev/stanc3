(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Errors

let parse parse_fun lexbuf =
  (* see the Menhir manual for the description of
     error messages support *)
  let open MenhirLib.General in
  let module Interp = Parser.MenhirInterpreter in
  let _ = Stack.push lexbuf Lexer.include_stack in
  let input _ =
    (Interp.lexer_lexbuf_to_supplier Lexer.token
       (Stack.top Lexer.include_stack))
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
          Some
            "Expected \"functions {\" or \"data {\" or \"transformed data {\" \
             or \"parameters {\" or \"transformed parameters {\" or \"model \
             {\" or \"generated quantities {\".\n"
        in
        raise
          (SyntaxError
             (Parsing
                ( message
                , Lexing.lexeme_start_p (Stack.top Lexer.include_stack)
                , Lexing.lexeme_end_p (Stack.top Lexer.include_stack) )))
    | (lazy (Cons (Interp.Element (state, _, start_pos, end_pos), _))) ->
        let message =
          try
            Some
              ( Parsing_errors.message (Interp.number state)
              ^
              if !Debug.grammar_logging then
                "(Parse error state "
                ^ string_of_int (Interp.number state)
                ^ ")"
              else "" )
          with Not_found ->
            Some
              ( if !Debug.grammar_logging then
                "(Parse error state "
                ^ string_of_int (Interp.number state)
                ^ ")"
              else "" )
        in
        raise (SyntaxError (Parsing (message, start_pos, end_pos)))
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

let%expect_test "parse conditional" =
  let ast =
    parse_string Parser.Incremental.program
      "model { if (1 < 2) { print(\"hi\");}}"
  in
  Core_kernel.print_s [%sexp (ast : Ast.untyped_program)] ;
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

let%expect_test "operator precedence" =
  let ast =
    parse_string Parser.Incremental.program
      "model { \n\
      \         \
       print({a,b?c:d||e&&f==g!=h<=i<j>=k>l+m-n*o/p%q\\r.*s./t^u[v]'});\n\
      \      }"
  in
  Core_kernel.print_s [%sexp (ast : Ast.untyped_program)] ;
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
                  (((expr_untyped
                     (Indexed
                      ((expr_untyped (Variable ((name a) (id_loc <opaque>))))
                       (expr_untyped_loc <opaque>))
                      ()))
                    (expr_untyped_loc <opaque>))
                   ((expr_untyped
                     (TernaryIf
                      ((expr_untyped
                        (Indexed
                         ((expr_untyped (Variable ((name b) (id_loc <opaque>))))
                          (expr_untyped_loc <opaque>))
                         ()))
                       (expr_untyped_loc <opaque>))
                      ((expr_untyped
                        (Indexed
                         ((expr_untyped (Variable ((name c) (id_loc <opaque>))))
                          (expr_untyped_loc <opaque>))
                         ()))
                       (expr_untyped_loc <opaque>))
                      ((expr_untyped
                        (BinOp
                         ((expr_untyped
                           (Indexed
                            ((expr_untyped (Variable ((name d) (id_loc <opaque>))))
                             (expr_untyped_loc <opaque>))
                            ()))
                          (expr_untyped_loc <opaque>))
                         Or
                         ((expr_untyped
                           (BinOp
                            ((expr_untyped
                              (Indexed
                               ((expr_untyped
                                 (Variable ((name e) (id_loc <opaque>))))
                                (expr_untyped_loc <opaque>))
                               ()))
                             (expr_untyped_loc <opaque>))
                            And
                            ((expr_untyped
                              (BinOp
                               ((expr_untyped
                                 (BinOp
                                  ((expr_untyped
                                    (Indexed
                                     ((expr_untyped
                                       (Variable ((name f) (id_loc <opaque>))))
                                      (expr_untyped_loc <opaque>))
                                     ()))
                                   (expr_untyped_loc <opaque>))
                                  Equals
                                  ((expr_untyped
                                    (Indexed
                                     ((expr_untyped
                                       (Variable ((name g) (id_loc <opaque>))))
                                      (expr_untyped_loc <opaque>))
                                     ()))
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
                                             (Indexed
                                              ((expr_untyped
                                                (Variable
                                                 ((name h) (id_loc <opaque>))))
                                               (expr_untyped_loc <opaque>))
                                              ()))
                                            (expr_untyped_loc <opaque>))
                                           Leq
                                           ((expr_untyped
                                             (Indexed
                                              ((expr_untyped
                                                (Variable
                                                 ((name i) (id_loc <opaque>))))
                                               (expr_untyped_loc <opaque>))
                                              ()))
                                            (expr_untyped_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))
                                        Less
                                        ((expr_untyped
                                          (Indexed
                                           ((expr_untyped
                                             (Variable
                                              ((name j) (id_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))
                                           ()))
                                         (expr_untyped_loc <opaque>))))
                                      (expr_untyped_loc <opaque>))
                                     Geq
                                     ((expr_untyped
                                       (Indexed
                                        ((expr_untyped
                                          (Variable ((name k) (id_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))
                                        ()))
                                      (expr_untyped_loc <opaque>))))
                                   (expr_untyped_loc <opaque>))
                                  Greater
                                  ((expr_untyped
                                    (BinOp
                                     ((expr_untyped
                                       (BinOp
                                        ((expr_untyped
                                          (Indexed
                                           ((expr_untyped
                                             (Variable
                                              ((name l) (id_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))
                                           ()))
                                         (expr_untyped_loc <opaque>))
                                        Plus
                                        ((expr_untyped
                                          (Indexed
                                           ((expr_untyped
                                             (Variable
                                              ((name m) (id_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))
                                           ()))
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
                                                (Indexed
                                                 ((expr_untyped
                                                   (Variable
                                                    ((name n) (id_loc <opaque>))))
                                                  (expr_untyped_loc <opaque>))
                                                 ()))
                                               (expr_untyped_loc <opaque>))
                                              Times
                                              ((expr_untyped
                                                (Indexed
                                                 ((expr_untyped
                                                   (Variable
                                                    ((name o) (id_loc <opaque>))))
                                                  (expr_untyped_loc <opaque>))
                                                 ()))
                                               (expr_untyped_loc <opaque>))))
                                            (expr_untyped_loc <opaque>))
                                           Divide
                                           ((expr_untyped
                                             (Indexed
                                              ((expr_untyped
                                                (Variable
                                                 ((name p) (id_loc <opaque>))))
                                               (expr_untyped_loc <opaque>))
                                              ()))
                                            (expr_untyped_loc <opaque>))))
                                         (expr_untyped_loc <opaque>))
                                        Modulo
                                        ((expr_untyped
                                          (BinOp
                                           ((expr_untyped
                                             (Indexed
                                              ((expr_untyped
                                                (Variable
                                                 ((name q) (id_loc <opaque>))))
                                               (expr_untyped_loc <opaque>))
                                              ()))
                                            (expr_untyped_loc <opaque>))
                                           LDivide
                                           ((expr_untyped
                                             (BinOp
                                              ((expr_untyped
                                                (BinOp
                                                 ((expr_untyped
                                                   (Indexed
                                                    ((expr_untyped
                                                      (Variable
                                                       ((name r) (id_loc <opaque>))))
                                                     (expr_untyped_loc <opaque>))
                                                    ()))
                                                  (expr_untyped_loc <opaque>))
                                                 EltTimes
                                                 ((expr_untyped
                                                   (Indexed
                                                    ((expr_untyped
                                                      (Variable
                                                       ((name s) (id_loc <opaque>))))
                                                     (expr_untyped_loc <opaque>))
                                                    ()))
                                                  (expr_untyped_loc <opaque>))))
                                               (expr_untyped_loc <opaque>))
                                              EltDivide
                                              ((expr_untyped
                                                (BinOp
                                                 ((expr_untyped
                                                   (Indexed
                                                    ((expr_untyped
                                                      (Variable
                                                       ((name t) (id_loc <opaque>))))
                                                     (expr_untyped_loc <opaque>))
                                                    ()))
                                                  (expr_untyped_loc <opaque>))
                                                 Exp
                                                 ((expr_untyped
                                                   (PostfixOp
                                                    ((expr_untyped
                                                      (Indexed
                                                       ((expr_untyped
                                                         (Variable
                                                          ((name u)
                                                           (id_loc <opaque>))))
                                                        (expr_untyped_loc <opaque>))
                                                       ((Single
                                                         ((expr_untyped
                                                           (Indexed
                                                            ((expr_untyped
                                                              (Variable
                                                               ((name v)
                                                                (id_loc <opaque>))))
                                                             (expr_untyped_loc
                                                              <opaque>))
                                                            ()))
                                                          (expr_untyped_loc
                                                           <opaque>))))))
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
                       (expr_untyped_loc <opaque>))))
                    (expr_untyped_loc <opaque>)))))
                (expr_untyped_loc <opaque>))))))
           (stmt_untyped_loc <opaque>)))))
       (generatedquantitiesblock ())) |}]

let parse_file parse_fun path =
  let chan = open_in path in
  let lexbuf =
    let open Lexing in
    let lexbuf = from_channel chan in
    lexbuf.lex_start_p
    <- {pos_fname= path; pos_lnum= 1; pos_bol= 0; pos_cnum= 0} ;
    lexbuf.lex_curr_p <- lexbuf.lex_start_p ;
    lexbuf
  in
  parse parse_fun lexbuf
