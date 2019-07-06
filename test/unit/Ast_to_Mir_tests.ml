open Frontend
open Middle
open Core_kernel
open Ast_to_Mir

let%expect_test "Operator-assign example" =
  let ast =
    Parse.parse_string Parser.Incremental.program
      {|
        model {
          real r;
          vector[2] x[4];
          x[1] ./= r;
        }
      |}
  in
  let semantic_check_program_exn ast =
    Option.value_exn
      (Result.ok
         (Semantic_check.semantic_check_program
            (Option.value_exn (Result.ok ast))))
  in
  let mir = trans_prog "" (semantic_check_program_exn ast) in
  print_s [%sexp (mir : typed_prog)] ;
  [%expect
    {|
      ((functions_block ()) (input_vars ()) (prepare_data ())
       (log_prob
        (((stmt
           (Block
            (((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id r)
                (decl_type (Sized SReal))))
              (smeta <opaque>))
             ((stmt
               (Decl (decl_adtype AutoDiffable) (decl_id x)
                (decl_type
                 (Sized
                  (SArray
                   (SVector
                    ((expr (Lit Int 2))
                     (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                   ((expr (Lit Int 4))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
              (smeta <opaque>))
             ((stmt
               (Assignment
                (x
                 ((Single
                   ((expr (Lit Int 1))
                    (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                ((expr
                  (FunApp StanLib EltDivide__
                   (((expr
                      (Indexed
                       ((expr (Var x))
                        (emeta
                         ((mtype (UArray UVector)) (mloc <opaque>)
                          (madlevel AutoDiffable))))
                       ((Single
                         ((expr (Lit Int 1))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))))
                     (emeta
                      ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))
                    ((expr (Var r))
                     (emeta
                      ((mtype UReal) (mloc <opaque>) (madlevel AutoDiffable)))))))
                 (emeta ((mtype UVector) (mloc <opaque>) (madlevel AutoDiffable))))))
              (smeta <opaque>)))))
          (smeta <opaque>))))
       (generate_quantities ()) (transform_inits ()) (output_vars ())
       (prog_name "") (prog_path "")) |}]

let mir_from_string s =
  let untyped_prog =
    Parse.parse_string Parser.Incremental.program s
    |> Result.map_error ~f:Parse.render_syntax_error
    |> Result.ok_or_failwith
  in
  let typed_prog_result = Semantic_check.semantic_check_program untyped_prog in
  let typed_prog =
    typed_prog_result
    |> Result.map_error ~f:(function
         | x :: _ -> (Semantic_error.pp |> Fmt.to_to_string) x
         | _ -> failwith "mir_from_string: can't happen" )
    |> Result.ok_or_failwith
  in
  trans_prog "" typed_prog

let%expect_test "Prefix-Op-Example" =
  let mir =
    mir_from_string
      {|
        model {
          int i;
          if (i < -1)
            print("Badger");
        }
      |}
  in
  let op = mir.log_prob in
  print_s [%sexp (op : stmt_loc list)] ;
  (* Perhaps this is producing too many nested lists. XXX*)
  [%expect
    {|
      ((Block
        ((Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt)))
         (IfElse
          (FunApp StanLib Less__ ((Var i) (FunApp StanLib PMinus__ ((Lit Int 1)))))
          (NRFunApp CompilerInternal FnPrint__ ((Lit Str Badger))) ())))) |}]

let%expect_test "read data" =
  let m = mir_from_string "data { matrix[10, 20] mat[5]; }" in
  print_s [%sexp (m.prepare_data : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype DataOnly) (decl_id mat)
      (decl_type
       (Sized (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5)))))
     (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
      (body
       (Block
        ((For (loopvar sym2__) (lower (Lit Int 1)) (upper (Lit Int 10))
          (body
           (Block
            ((For (loopvar sym3__) (lower (Lit Int 1)) (upper (Lit Int 20))
              (body
               (Block
                ((Assignment
                  (mat
                   ((Single (Var sym1__)) (Single (Var sym2__))
                    (Single (Var sym3__))))
                  (Indexed
                   (FunApp CompilerInternal FnReadData__
                    ((Lit Str mat) (Lit Str matrix) (Lit Int 10) (Lit Int 20)))
                   ((Single (Var sym1__)) (Single (Var sym2__))
                    (Single (Var sym3__)))))))))))))))))) |}]

let%expect_test "read param" =
  let m = mir_from_string "parameters { matrix<lower=0>[10, 20] mat[5]; }" in
  print_s [%sexp (m.log_prob : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype AutoDiffable) (decl_id mat)
      (decl_type
       (Sized (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5)))))
     (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
      (body
       (Block
        ((Assignment (mat ((Single (Var sym1__))))
          (Indexed
           (FunApp CompilerInternal FnReadParam__
            ((Lit Str mat) (Lit Str matrix) (Lit Int 10) (Lit Int 20)))
           ((Single (Var sym1__)))))))))
     (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
      (body
       (Block
        ((For (loopvar sym2__) (lower (Lit Int 1)) (upper (Lit Int 10))
          (body
           (Block
            ((For (loopvar sym3__) (lower (Lit Int 1)) (upper (Lit Int 20))
              (body
               (Block
                ((Assignment
                  (mat
                   ((Single (Var sym1__)) (Single (Var sym2__))
                    (Single (Var sym3__))))
                  (FunApp CompilerInternal FnConstrain__
                   ((Indexed (Var mat)
                     ((Single (Var sym1__)) (Single (Var sym2__))
                      (Single (Var sym3__))))
                    (Lit Str lb) (Lit Int 0))))))))))))))))) |}]

let%expect_test "gen quant" =
  let m =
    mir_from_string "generated quantities { matrix<lower=0>[10, 20] mat[5]; }"
  in
  print_s [%sexp (m.generate_quantities : stmt_loc list)] ;
  [%expect
    {|
    ((Decl (decl_adtype DataOnly) (decl_id mat)
      (decl_type
       (Sized (SArray (SMatrix (Lit Int 10) (Lit Int 20)) (Lit Int 5)))))
     (IfElse (Var emit_generated_quantities__)
      (Block
       ((For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
         (body
          (Block
           ((NRFunApp CompilerInternal FnCheck__
             ((Lit Str greater_or_equal) (Lit Str mat[sym1__])
              (Indexed (Var mat) ((Single (Var sym1__)))) (Lit Int 0)))))))
        (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
         (body
          (Block
           ((For (loopvar sym2__) (lower (Lit Int 1)) (upper (Lit Int 10))
             (body
              (Block
               ((For (loopvar sym3__) (lower (Lit Int 1)) (upper (Lit Int 20))
                 (body
                  (Block
                   ((NRFunApp CompilerInternal FnWriteParam__
                     ((Indexed (Var mat)
                       ((Single (Var sym1__)) (Single (Var sym2__))
                        (Single (Var sym3__))))))))))))))))))))
      ())) |}]
