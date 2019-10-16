open Frontend
open Middle
open Core_kernel
open Ast_to_Mir

let%expect_test "Operator-assign example" =
  Frontend_utils.typed_ast_of_string_exn
    {|
        model {
          real r;
          vector[2] x[4];
          x[1] ./= r;
        }
      |}
  |> fst |> trans_prog ""
  |> (fun {log_prob; _} -> log_prob)
  |> Fmt.strf "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut Pretty.pp_stmt_loc)
  |> print_endline ;
  [%expect
    {|
      {
        real r;
        array[vector[2], 4] x;
        x[1] = (x[1] ./ r);
      } |}]

let mir_from_string s =
  Frontend_utils.typed_ast_of_string_exn s |> fst |> trans_prog ""

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
      (((stmt
         (Block
          (((stmt
             (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt))))
            (smeta <opaque>))
           ((stmt
             (IfElse
              ((expr
                (FunApp StanLib Less__
                 (((expr (Var i))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                  ((expr
                    (FunApp StanLib PMinus__
                     (((expr (Lit Int 1))
                       (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
                   (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
               (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
              ((stmt
                (NRFunApp CompilerInternal FnPrint__
                 (((expr (Lit Str Badger))
                   (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly)))))))
               (smeta <opaque>))
              ()))
            (smeta <opaque>)))))
        (smeta <opaque>))) |}]

let%expect_test "read data" =
  let m = mir_from_string "data { matrix[10, 20] mat[5]; }" in
  print_s [%sexp (m.prepare_data : stmt_loc list)] ;
  [%expect
    {|
    (((stmt
       (Decl (decl_adtype DataOnly) (decl_id mat)
        (decl_type
         (Sized
          (SArray
           (SMatrix
            ((expr (Lit Int 10))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
            ((expr (Lit Int 20))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
           ((expr (Lit Int 5))
            (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
      (smeta <opaque>))) |}]

let%expect_test "read param" =
  let m = mir_from_string "parameters { matrix<lower=0>[10, 20] mat[5]; }" in
  print_s [%sexp (m.log_prob : stmt_loc list)] ;
  [%expect
    {|
    (((stmt
       (Decl (decl_adtype AutoDiffable) (decl_id mat)
        (decl_type
         (Sized
          (SArray
           (SMatrix
            ((expr (Lit Int 10))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
            ((expr (Lit Int 20))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
           ((expr (Lit Int 5))
            (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
      (smeta <opaque>))
     ((stmt
       (For (loopvar sym1__)
        (lower
         ((expr (Lit Int 1))
          (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
        (upper
         ((expr (Lit Int 5))
          (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
        (body
         ((stmt
           (Block
            (((stmt
               (For (loopvar sym2__)
                (lower
                 ((expr (Lit Int 1))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (upper
                 ((expr (Lit Int 10))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                (body
                 ((stmt
                   (Block
                    (((stmt
                       (For (loopvar sym3__)
                        (lower
                         ((expr (Lit Int 1))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                        (upper
                         ((expr (Lit Int 20))
                          (emeta
                           ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
                        (body
                         ((stmt
                           (Block
                            (((stmt
                               (Assignment
                                (mat (UArray UMatrix)
                                 ((Single
                                   ((expr (Var sym1__))
                                    (emeta
                                     ((mtype UInt) (mloc <opaque>)
                                      (madlevel DataOnly)))))
                                  (Single
                                   ((expr (Var sym2__))
                                    (emeta
                                     ((mtype UInt) (mloc <opaque>)
                                      (madlevel DataOnly)))))
                                  (Single
                                   ((expr (Var sym3__))
                                    (emeta
                                     ((mtype UInt) (mloc <opaque>)
                                      (madlevel DataOnly)))))))
                                ((expr
                                  (FunApp CompilerInternal FnConstrain__
                                   (((expr
                                      (Indexed
                                       ((expr (Var mat))
                                        (emeta
                                         ((mtype (UArray UMatrix))
                                          (mloc <opaque>)
                                          (madlevel AutoDiffable))))
                                       ((Single
                                         ((expr (Var sym1__))
                                          (emeta
                                           ((mtype UInt) (mloc <opaque>)
                                            (madlevel DataOnly)))))
                                        (Single
                                         ((expr (Var sym2__))
                                          (emeta
                                           ((mtype UInt) (mloc <opaque>)
                                            (madlevel DataOnly)))))
                                        (Single
                                         ((expr (Var sym3__))
                                          (emeta
                                           ((mtype UInt) (mloc <opaque>)
                                            (madlevel DataOnly))))))))
                                     (emeta
                                      ((mtype UReal) (mloc <opaque>)
                                       (madlevel AutoDiffable))))
                                    ((expr (Lit Str lb))
                                     (emeta
                                      ((mtype UReal) (mloc <opaque>)
                                       (madlevel DataOnly))))
                                    ((expr (Lit Int 0))
                                     (emeta
                                      ((mtype UInt) (mloc <opaque>)
                                       (madlevel DataOnly)))))))
                                 (emeta
                                  ((mtype UReal) (mloc <opaque>)
                                   (madlevel AutoDiffable))))))
                              (smeta <opaque>)))))
                          (smeta <opaque>)))))
                      (smeta <opaque>)))))
                  (smeta <opaque>)))))
              (smeta <opaque>)))))
          (smeta <opaque>)))))
      (smeta <opaque>))) |}]

let%expect_test "gen quant" =
  let m =
    mir_from_string "generated quantities { matrix<lower=0>[10, 20] mat[5]; }"
  in
  print_s [%sexp (m.generate_quantities : stmt_loc list)] ;
  [%expect
    {|
    (((stmt
       (IfElse
        ((expr
          (FunApp StanLib PNot__
           (((expr
              (EOr
               ((expr (Var emit_transformed_parameters__))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
               ((expr (Var emit_generated_quantities__))
                (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
        ((stmt (Return ())) (smeta <opaque>)) ()))
      (smeta <opaque>))
     ((stmt
       (IfElse
        ((expr
          (FunApp StanLib PNot__
           (((expr (Var emit_generated_quantities__))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
         (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
        ((stmt (Return ())) (smeta <opaque>)) ()))
      (smeta <opaque>))
     ((stmt
       (Decl (decl_adtype DataOnly) (decl_id mat)
        (decl_type
         (Sized
          (SArray
           (SMatrix
            ((expr (Lit Int 10))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
            ((expr (Lit Int 20))
             (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
           ((expr (Lit Int 5))
            (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))))
      (smeta <opaque>))
     ((stmt
       (For (loopvar sym1__)
        (lower
         ((expr (Lit Int 1))
          (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
        (upper
         ((expr (Lit Int 5))
          (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))
        (body
         ((stmt
           (Block
            (((stmt
               (NRFunApp CompilerInternal FnCheck__
                (((expr (Lit Str greater_or_equal))
                  (emeta ((mtype UReal) (mloc <opaque>) (madlevel DataOnly))))
                 ((expr (Lit Str mat[sym1__]))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))
                 ((expr
                   (Indexed
                    ((expr (Var mat))
                     (emeta
                      ((mtype (UArray UMatrix)) (mloc <opaque>)
                       (madlevel DataOnly))))
                    ((Single
                      ((expr (Var sym1__))
                       (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly))))))))
                  (emeta ((mtype UMatrix) (mloc <opaque>) (madlevel DataOnly))))
                 ((expr (Lit Int 0))
                  (emeta ((mtype UInt) (mloc <opaque>) (madlevel DataOnly)))))))
              (smeta <opaque>)))))
          (smeta <opaque>)))))
      (smeta <opaque>))) |}]
