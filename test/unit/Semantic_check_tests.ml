open Core_kernel
open Frontend
open Frontend_utils

let%expect_test "indexed type checking" =
  {|
transformed data {
    int indices[3] = {1, 2, 3};
    matrix[3, 4] mat[5];
    print(mat[indices, :, indices][2,1,1]);
}
|}
  |> typed_ast_of_string_exn
  |> Fmt.strf "@[<v>%a@]" Pretty_printing.pp_program
  |> print_endline ;
  [%expect
    {|
    transformed data {
      int indices[3] = {1, 2, 3};
      matrix[3, 4] mat[5];
      print(mat[indices,  : , indices][2, 1, 1]);
    } |}]

let%expect_test "invalid StaticSparseMatrix decl location" =
  let prog =
    {|
    data {
      sparse_matrix[{1, 2, 3}, {1, 2, 3}, 10 ,10] mat;
    }
    transformed data {
      sparse_matrix[{1, 2, 3}, {1, 2, 3}, 10 ,10] mat1;
    }
    |}
  in
  (try let _ = typed_ast_of_string_exn prog in ()
   with
   | Failure e -> print_endline e);
  [%expect
    {|
    Incorrect sparse_matrix declaration in the Transformed Data block; Declerations with known nonzero values can only be defined in the data or parameters blocks. |}]
