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
  |> (fun Program.({log_prob; _}) -> log_prob)
  |> Fmt.strf "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut Stmt.Located.pp)
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
  print_s [%sexp (op : Stmt.Located.t list)] ;
  (* Perhaps this is producing too many nested lists. XXX*)
  [%expect
    {|
      (((pattern
         (Block
          (((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt))))
            (meta <opaque>))
           ((pattern
             (IfElse
              ((pattern
                (FunApp (StanLib Less__ FnPlain AoS)
                 (((pattern (Var i))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
                  ((pattern
                    (FunApp (StanLib PMinus__ FnPlain AoS)
                     (((pattern (Lit Int 1))
                       (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
                   (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
              ((pattern
                (NRFunApp (CompilerInternal FnPrint)
                 (((pattern (Lit Str Badger))
                   (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
               (meta <opaque>))
              ()))
            (meta <opaque>)))))
        (meta <opaque>))) |}]

let%expect_test "read data" =
  let m = mir_from_string "data { matrix[10, 20] mat[5]; }" in
  print_s [%sexp (m.prepare_data : Stmt.Located.t list)] ;
  [%expect
    {|
    (((pattern
       (Decl (decl_adtype DataOnly) (decl_id mat)
        (decl_type
         (Sized
          (SArray
           (SMatrix AoS
            ((pattern (Lit Int 10))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern (Lit Int 20))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
           ((pattern (Lit Int 5))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))))
      (meta <opaque>))) |}]

let%expect_test "read param" =
  let m = mir_from_string "parameters { matrix<lower=0>[10, 20] mat[5]; }" in
  print_s [%sexp (m.log_prob : Stmt.Located.t list)] ;
  [%expect
    {|
    (((pattern
       (Decl (decl_adtype AutoDiffable) (decl_id mat)
        (decl_type
         (Sized
          (SArray
           (SMatrix AoS
            ((pattern (Lit Int 10))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern (Lit Int 20))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
           ((pattern (Lit Int 5))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))))
      (meta <opaque>))) |}]

let%expect_test "gen quant" =
  let m =
    mir_from_string "generated quantities { matrix<lower=0>[10, 20] mat[5]; }"
  in
  print_s [%sexp (m.generate_quantities : Stmt.Located.t list)] ;
  [%expect
    {|
    (((pattern
       (IfElse
        ((pattern
          (FunApp (StanLib PNot__ FnPlain AoS)
           (((pattern
              (EOr
               ((pattern (Var emit_transformed_parameters__))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
               ((pattern (Var emit_generated_quantities__))
                (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
         (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
        ((pattern (Return ())) (meta <opaque>)) ()))
      (meta <opaque>))
     ((pattern
       (IfElse
        ((pattern
          (FunApp (StanLib PNot__ FnPlain AoS)
           (((pattern (Var emit_generated_quantities__))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
         (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
        ((pattern (Return ())) (meta <opaque>)) ()))
      (meta <opaque>))
     ((pattern
       (Decl (decl_adtype DataOnly) (decl_id mat)
        (decl_type
         (Sized
          (SArray
           (SMatrix AoS
            ((pattern (Lit Int 10))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))
            ((pattern (Lit Int 20))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
           ((pattern (Lit Int 5))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))))
      (meta <opaque>))
     ((pattern
       (For (loopvar sym1__)
        (lower
         ((pattern (Lit Int 1))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
        (upper
         ((pattern (Lit Int 5))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
        (body
         ((pattern
           (Block
            (((pattern
               (For (loopvar sym2__)
                (lower
                 ((pattern (Lit Int 1))
                  (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
                (upper
                 ((pattern (Lit Int 10))
                  (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
                (body
                 ((pattern
                   (Block
                    (((pattern
                       (For (loopvar sym3__)
                        (lower
                         ((pattern (Lit Int 1))
                          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
                        (upper
                         ((pattern (Lit Int 20))
                          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))
                        (body
                         ((pattern
                           (Block
                            (((pattern
                               (NRFunApp
                                (CompilerInternal (FnCheck greater_or_equal))
                                (((pattern
                                   (Lit Str "mat[sym1__, sym2__, sym3__]"))
                                  (meta
                                   ((type_ UInt) (loc <opaque>)
                                    (adlevel DataOnly))))
                                 ((pattern
                                   (Indexed
                                    ((pattern (Var mat))
                                     (meta
                                      ((type_ (UArray UMatrix)) (loc <opaque>)
                                       (adlevel DataOnly))))
                                    ((Single
                                      ((pattern (Var sym1__))
                                       (meta
                                        ((type_ UInt) (loc <opaque>)
                                         (adlevel DataOnly)))))
                                     (Single
                                      ((pattern (Var sym2__))
                                       (meta
                                        ((type_ UInt) (loc <opaque>)
                                         (adlevel DataOnly)))))
                                     (Single
                                      ((pattern (Var sym3__))
                                       (meta
                                        ((type_ UInt) (loc <opaque>)
                                         (adlevel DataOnly))))))))
                                  (meta
                                   ((type_ UReal) (loc <opaque>)
                                    (adlevel DataOnly))))
                                 ((pattern (Lit Int 0))
                                  (meta
                                   ((type_ UInt) (loc <opaque>)
                                    (adlevel DataOnly)))))))
                              (meta <opaque>)))))
                          (meta <opaque>)))))
                      (meta <opaque>)))))
                  (meta <opaque>)))))
              (meta <opaque>)))))
          (meta <opaque>)))))
      (meta <opaque>))) |}]
