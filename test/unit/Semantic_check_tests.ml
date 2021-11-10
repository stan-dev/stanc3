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
  |> typed_ast_of_string_exn |> Ast.untyped_program_of_typed_program
  |> Fmt.strf "@[<v>%a@]" (Pretty_printing.pp_program ~bare_functions:false)
  |> print_endline ;
  [%expect
    {|
    transformed data {
      array[3] int indices = {1, 2, 3};
      array[5] matrix[3, 4] mat;
      print(mat[indices,  : , indices][2, 1, 1]);
    } |}]
