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
  |> trans_prog ""
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
  Frontend_utils.typed_ast_of_string_exn s |> trans_prog ""

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
                  (mat (UArray UMatrix)
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
        ((Assignment (mat (UArray UMatrix) ((Single (Var sym1__))))
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
               (Assignment
                (mat (UArray UMatrix)
                 ((Single (Var sym1__)) (Single (Var sym2__))
                  (Single (Var sym3__))))
                (FunApp CompilerInternal FnConstrain__
                 ((FunApp CompilerInternal FnMatrixElement__
                   ((Indexed (Var mat) ((Single (Var sym1__)))) (Var sym2__)
                    (Var sym3__)))
                  (Lit Str lb) (Lit Int 0))))))))))))))) |}]

let%expect_test "gen quant" =
  let m =
    mir_from_string "generated quantities { matrix<lower=0>[10, 20] mat[5]; }"
  in
  print_s [%sexp (m.generate_quantities : stmt_loc list)] ;
  [%expect
    {|
    ((IfElse (Var emit_generated_quantities__)
      (Block
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
                  (NRFunApp CompilerInternal FnWriteParam__
                   ((FunApp CompilerInternal FnMatrixElement__
                     ((Indexed (Var mat) ((Single (Var sym1__)))) (Var sym2__)
                      (Var sym3__)))))))))))))))
        (For (loopvar sym1__) (lower (Lit Int 1)) (upper (Lit Int 5))
         (body
          (Block
           ((NRFunApp CompilerInternal FnCheck__
             ((Lit Str greater_or_equal) (Lit Str mat[sym1__])
              (Indexed (Var mat) ((Single (Var sym1__)))) (Lit Int 0)))))))))
      ())) |}]
