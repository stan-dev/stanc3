open Core_kernel
open Frontend

let%expect_test "pull out multi indices" =
  let {Ast.transformeddatablock= td; _} =
    Frontend_utils.typed_ast_of_string_exn
      {| transformed data {
    int indices[3] = {1, 2, 3};
    matrix[20, 21] mat[30];
    mat = mat[indices, indices];
} |}
  in
  let lst_stmt = List.last_exn (Option.value_exn td) in
  let rhs =
    match lst_stmt.Ast.stmt with
    | Ast.Assignment {assign_rhs; _} -> assign_rhs
    | _ -> raise_s [%message "Didn't get an assignment"]
  in
  let new_stmts = ref [] in
  let res = Desugar.pull_new_multi_indices_expr new_stmts rhs in
  print_endline
    Fmt.(strf "replacement expr: @[<v>%a@]" Pretty_printing.pp_expression res) ;
  print_endline
    Fmt.(
      strf "AST statements:@, @[<v>%a@]"
        (list ~sep:cut Pretty_printing.pp_statement)
        !new_stmts) ;
  [%expect
    {|
    replacement expr: sym1__
    AST statements:
     matrix sym1__;
     sym1__ = FnResizeToMatch__(sym1__, indices);
     for (sym2__ in indices) {
       sym1__[sym2__] = FnResizeToMatch__(sym1__[sym2__], indices);
       for (sym3__ in indices) {
         sym1__[sym2__, sym3__] = mat[indices[sym2__], indices[sym3__]];
       }
     } |}]

let%expect_test "desugar multi" =
  Frontend_utils.typed_ast_of_string_exn
    {|
transformed data {
  int indices[3] = {2, 1, 3};
  vector[3] vec[5];
  print(vec[indices]);
} |}
  |> Desugar.desugar_prog
  |> (fun p ->
       Fmt.pr "@[<v>%a@]\n" Pretty_printing.pp_program p ;
       p )
  |> (fun {Ast.transformeddatablock= td; _} -> td)
  |> Option.value_exn |> Fn.flip List.drop 3
  |> List.iter ~f:(Fn.compose print_s Ast.sexp_of_typed_statement) ;
  [%expect
    {|
    transformed data {
      int indices[3] = {2, 1, 3};
      vector[3] vec[5];
      vector sym2__;
      sym2__ = FnResizeToMatch__(sym2__, indices);
      for (sym3__ in indices) {
        sym2__[sym3__] = vec[indices[sym3__]];
      }
      print(sym2__);
    }

    ((stmt
      (Assignment
       (assign_lhs
        ((assign_identifier ((name sym2__) (id_loc <opaque>)))
         (assign_indices ())
         (assign_meta
          ((loc <opaque>) (id_ad_level DataOnly) (id_type_ (UArray UVector))
           (lhs_ad_level DataOnly) (lhs_type_ (UArray UVector))))))
       (assign_op Assign)
       (assign_rhs
        ((expr
          (FunApp CompilerInternal ((name FnResizeToMatch__) (id_loc <opaque>))
           (((expr
              (Indexed
               ((expr (Variable ((name sym2__) (id_loc <opaque>))))
                (emeta
                 ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UVector)))))
               ()))
             (emeta
              ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UVector)))))
            ((expr (Variable ((name indices) (id_loc <opaque>))))
             (emeta ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UInt))))))))
         (emeta ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UVector))))))))
     (smeta ((loc <opaque>) (return_type NoReturnType))))
    ((stmt
      (ForEach ((name sym3__) (id_loc <opaque>))
       ((expr (Variable ((name indices) (id_loc <opaque>))))
        (emeta ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UInt)))))
       ((stmt
         (Block
          (((stmt
             (Assignment
              (assign_lhs
               ((assign_identifier ((name sym2__) (id_loc <opaque>)))
                (assign_indices
                 ((Single
                   ((expr (Variable ((name sym3__) (id_loc <opaque>))))
                    (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt)))))))
                (assign_meta
                 ((loc <opaque>) (id_ad_level DataOnly)
                  (id_type_ (UArray UVector)) (lhs_ad_level DataOnly)
                  (lhs_type_ (UArray UVector))))))
              (assign_op Assign)
              (assign_rhs
               ((expr
                 (Indexed
                  ((expr (Variable ((name vec) (id_loc <opaque>))))
                   (emeta
                    ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UVector)))))
                  ((Single
                    ((expr
                      (Indexed
                       ((expr (Variable ((name indices) (id_loc <opaque>))))
                        (emeta
                         ((loc <opaque>) (ad_level DataOnly)
                          (type_ (UArray UInt)))))
                       ((Single
                         ((expr (Variable ((name sym3__) (id_loc <opaque>))))
                          (emeta
                           ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))))))
                     (emeta ((loc <opaque>) (ad_level DataOnly) (type_ UInt))))))))
                (emeta
                 ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UVector))))))))
            (smeta ((loc <opaque>) (return_type NoReturnType)))))))
        (smeta ((loc <opaque>) (return_type NoReturnType))))))
     (smeta ((loc <opaque>) (return_type NoReturnType))))
    ((stmt
      (Print
       ((PExpr
         ((expr (Variable ((name sym2__) (id_loc <opaque>))))
          (emeta ((loc <opaque>) (ad_level DataOnly) (type_ (UArray UVector)))))))))
     (smeta ((loc <opaque>) (return_type NoReturnType)))) |}]

let%expect_test "desugar row" =
  Frontend_utils.typed_ast_of_string_exn
    {|
transformed data {
  matrix[3,4] mat[5];
  print(mat[1, 2]);
} |}
  |> Desugar.desugar_prog
  |> Fmt.pr "@[<v>%a@]\n" Pretty_printing.pp_program ;
  [%expect
    {|
    transformed data {
       matrix[3, 4] mat[5];
       print(row(mat[1], 2));
     } |}]

let%expect_test "desugar matrixelement" =
  Frontend_utils.typed_ast_of_string_exn
    {|
transformed data {
  matrix[3,4] mat[5, 6];
  print(mat[1, 2, 3, 4]);
} |}
  |> Desugar.desugar_prog
  |> Fmt.pr "@[<v>%a@]\n" Pretty_printing.pp_program ;
  [%expect
    {|
    transformed data {
        matrix[3, 4] mat[5, 6];
        print(FnMatrixElement__(mat[1][2], 3, 4));
      } |}]

let%expect_test "desugar matrixelement arr" =
  Frontend_utils.typed_ast_of_string_exn
    {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat[5];
  int i = 1;
  int j = 2;
  for (k in 1:size(arr))
    mat[i, j, k] = mat[arr[i], j, arr[k]];
} |}
  |> Desugar.desugar_prog
  |> Fmt.pr "@[<v>%a@]\n" Pretty_printing.pp_program ;
  [%expect
    {|
    transformed data {
         int arr[3] = {2, 3, 1};
         matrix[3, 4] mat[5];
         int i = 1;
         int j = 2;
         for (k in 1 : size(arr| ))
           mat[i, j, k] = FnMatrixElement__(mat[arr[i]], j, arr[k]);
       } |}]

let%expect_test "matrix array multi indexing " =
  Frontend_utils.typed_ast_of_string_exn
    {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat;
  print(mat[arr, 2]);
} |}
  |> Desugar.desugar_prog
  |> Fmt.pr "@[<v>%a@]\n" Pretty_printing.pp_program ;
  [%expect
    {|
    transformed data {
          int arr[3] = {2, 3, 1};
          matrix[3, 4] mat;
          print(mat[arr[2]]);
        } |}]

let%expect_test "matrix array multi indexing " =
  Frontend_utils.typed_ast_of_string_exn
    {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat[5];
  print(mat[2, arr, 2]);
} |}
  |> Desugar.desugar_prog
  |> Fmt.pr "@[<v>%a@]\n" Pretty_printing.pp_program ;
  [%expect
    {|
    transformed data {
           int arr[3] = {2, 3, 1};
           matrix[3, 4] mat[5];
           print(mat[2, arr[2]]);
         } |}]

let%expect_test "matrix array multi indexing " =
  Frontend_utils.typed_ast_of_string_exn
    {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat[5];
  print(mat[2, arr, 2]);
} |}
  |> Desugar.desugar_prog
  |> Fmt.pr "@[<v>%a@]\n" Pretty_printing.pp_program ;
  [%expect
    {|
    transformed data {
            int arr[3] = {2, 3, 1};
            matrix[3, 4] mat[5];
            print(mat[2, arr[2]]);
          } |}]
