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
  let ast = parse_string Parser.Incremental.program "model { if (1 < 2) { print(\"hi\");}}" in
  Core_kernel.print_s [%sexp (ast: Ast.untyped_program)];
  [%expect {|
    ((functionblock ()) (datablock ()) (transformeddatablock ())
     (parametersblock ()) (transformedparametersblock ())
     (modelblock
      (((UntypedStmt
         ((IfThenElse
           (UntypedExpr
            ((InfixOp
              (UntypedExpr ((IntNumeral 1) ((expr_untyped_meta_loc <opaque>))))
              Less
              (UntypedExpr ((IntNumeral 2) ((expr_untyped_meta_loc <opaque>)))))
             ((expr_untyped_meta_loc <opaque>))))
           (UntypedStmt
            ((Block
              ((UntypedStmt
                ((Print ((PString "\"hi\""))) ((stmt_untyped_meta_loc <opaque>))))))
             ((stmt_untyped_meta_loc <opaque>))))
           ())
          ((stmt_untyped_meta_loc <opaque>)))))))
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
