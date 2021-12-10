open Core_kernel
open Frontend
open Frontend_utils

let%expect_test "indexed type checking" =
  {|
transformed data {
    array[3] int indices = {1, 2, 3};
    array[5] matrix[3, 4] mat;
    print(mat[indices, :, indices][2,1,1]);
}
|}
  |> typed_ast_of_string_exn |> Pretty_printing.pretty_print_typed_program
  |> print_endline ;
  [%expect
    {|
    transformed data {
      array[3] int indices = {1, 2, 3};
      array[5] matrix[3, 4] mat;
      print(mat[indices,  : , indices][2, 1, 1]);
    } |}]
