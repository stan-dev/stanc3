open Middle
open Core

let%expect_test "Operator-assign example" =
  Test_utils.mir_of_string
    {|
        model {
          real r;
          array[4] vector[2] x;
          x[1] ./= r;
        }
      |}
  |> (fun Program.{log_prob; _} -> log_prob)
  |> Fmt.str "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut Stmt.Located.pp)
  |> print_endline;
  [%expect
    {|
      {
        real r;
        array[vector[2], 4] x;
        x[1] = (x[1] ./ r);
      } |}]

let%expect_test "Prefix-Op-Example" =
  let mir =
    Test_utils.mir_of_string
      {|
        model {
          int i;
          if (i < -1)
            print("Badger");
        }
      |}
  in
  let op = mir.log_prob in
  print_s [%sexp (op : Stmt.Located.t list)];
  (* Perhaps this is producing too many nested lists. XXX*)
  [%expect
    {|
      (((pattern
         (Block
          (((pattern
             (Decl (decl_adtype AutoDiffable) (decl_id i) (decl_type (Sized SInt))
              (initialize true)))
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
  let m = Test_utils.mir_of_string "data { array[5] matrix[10, 20] mat; }" in
  print_s [%sexp (m.prepare_data : Stmt.Located.t list)];
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
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
        (initialize true)))
      (meta <opaque>))) |}]

let%expect_test "read param" =
  let m =
    Test_utils.mir_of_string
      "parameters { array[5] matrix<lower=0>[10, 20] mat; }" in
  print_s [%sexp (m.log_prob : Stmt.Located.t list)];
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
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
        (initialize true)))
      (meta <opaque>))) |}]

let%expect_test "gen quant" =
  let m =
    Test_utils.mir_of_string
      "generated quantities { array[5] matrix<lower=0>[10, 20] mat; }" in
  print_s [%sexp (m.generate_quantities : Stmt.Located.t list)];
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
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
        (initialize true)))
      (meta <opaque>))
     ((pattern
       (NRFunApp
        (CompilerInternal
         (FnCheck
          (trans
           (Lower
            ((pattern (Lit Int 0))
             (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))
          (var_name mat)
          (var
           ((pattern (Var mat))
            (meta ((type_ (UArray UMatrix)) (loc <opaque>) (adlevel DataOnly)))))))
        (((pattern (Lit Int 0))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
      (meta <opaque>))) |}]

let%expect_test "read data - constraint " =
  let m = Test_utils.mir_of_string "data { array[5] real<lower=3.4> y; }" in
  print_s [%sexp (m.prepare_data : Stmt.Located.t list)];
  [%expect
    {|
  (((pattern
     (Decl (decl_adtype DataOnly) (decl_id y)
      (decl_type
       (Sized
        (SArray SReal
         ((pattern (Lit Int 5))
          (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
      (initialize true)))
    (meta <opaque>))
   ((pattern
     (NRFunApp
      (CompilerInternal
       (FnCheck
        (trans
         (Lower
          ((pattern (Lit Real 3.4))
           (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly))))))
        (var_name y)
        (var
         ((pattern (Var y))
          (meta ((type_ (UArray UReal)) (loc <opaque>) (adlevel DataOnly)))))))
      (((pattern (Lit Real 3.4))
        (meta ((type_ UReal) (loc <opaque>) (adlevel DataOnly)))))))
    (meta <opaque>))) |}]

let%expect_test "read data - tuple" =
  let m =
    Test_utils.mir_of_string "data { array[5] tuple(real, simplex[20]) x; }"
  in
  print_s [%sexp (m.prepare_data : Stmt.Located.t list)];
  [%expect
    {|
    (((pattern
       (Decl (decl_adtype (TupleAD (DataOnly DataOnly))) (decl_id x)
        (decl_type
         (Sized
          (SArray
           (STuple
            (SReal
             (SVector AoS
              ((pattern (Lit Int 20))
               (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
           ((pattern (Lit Int 5))
            (meta ((type_ UInt) (loc <opaque>) (adlevel DataOnly)))))))
        (initialize true)))
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
               (Block
                (((pattern
                   (NRFunApp
                    (CompilerInternal
                     (FnCheck (trans Simplex) (var_name x[sym1__].2)
                      (var
                       ((pattern
                         (TupleProjection
                          ((pattern
                            (Indexed
                             ((pattern (Var x))
                              (meta
                               ((type_ (UArray (UTuple (UReal UVector))))
                                (loc <opaque>)
                                (adlevel (TupleAD (DataOnly DataOnly))))))
                             ((Single
                               ((pattern (Var sym1__))
                                (meta
                                 ((type_ UInt) (loc <opaque>) (adlevel DataOnly))))))))
                           (meta
                            ((type_ (UTuple (UReal UVector))) (loc <opaque>)
                             (adlevel (TupleAD (DataOnly DataOnly))))))
                          2))
                        (meta
                         ((type_ UVector) (loc <opaque>)
                          (adlevel (TupleAD (DataOnly DataOnly)))))))))
                    ()))
                  (meta <opaque>)))))
              (meta <opaque>)))))
          (meta <opaque>)))))
      (meta <opaque>))) |}]
