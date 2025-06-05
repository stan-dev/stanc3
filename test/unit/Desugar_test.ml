open Core
open Analysis_and_optimization

let print_tdata Middle.Program.{prepare_data; _} =
  Fmt.(str "@[<v>%a@]@," (list ~sep:cut Middle.Stmt.Fixed.pp) prepare_data)
  |> print_endline

let%expect_test "matrix array multi indexing " =
  Test_utils.mir_of_string
    {|
transformed data {
  array[3] int arr = {2, 3, 1};
  array[5] matrix[3,4] mat;
  print(mat[2, arr, 2]);
} |}
  |> Partial_evaluator.eval_prog |> print_tdata;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[2, arr, 2]); |}]

let%expect_test "matrix array multi indexing " =
  Test_utils.mir_of_string
    {|
transformed data {
  array[3] int arr = {2, 3, 1};
  array[5] matrix[3,4] mat;
  print(mat[2][arr][2]);
} |}
  |> Partial_evaluator.eval_prog |> print_tdata;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[2][arr[2]]);
 |}]

let%expect_test "matrix array multi indexing " =
  Test_utils.mir_of_string
    {|
transformed data {
  array[3] int arr = {2, 3, 1};
  array[5] matrix[3,4] mat;
  print(mat[2, arr, arr][2, 2]);
} |}
  |> Partial_evaluator.eval_prog |> print_tdata;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[2, arr[2], arr[2]]);
 |}]

let%expect_test "matrix array multi indexing " =
  Test_utils.mir_of_string
    {|
transformed data {
  array[3] int arr = {2, 3, 1};
  array[5] matrix[3,4] mat;
  print(mat[3:, 2:3][2, 1]);
} |}
  |> Partial_evaluator.eval_prog |> print_tdata;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[4, 2]);
 |}]

let%expect_test "matrix array multi indexing " =
  Test_utils.mir_of_string
    {|
transformed data {
  array[3] int arr = {2, 3, 1};
  array[5] matrix[3,4] mat;
  print(mat[:3, 1, :]);
} |}
  |> Partial_evaluator.eval_prog |> print_tdata;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[1:3, 1]);
 |}]

let%expect_test "matrix array multi indexing " =
  Test_utils.mir_of_string
    {|
transformed data {
  array[3] int arr = {2, 3, 1};
  array[5] matrix[3, 4] mat;
  print(mat[4:, 2:3, 1]);
  print(mat[:, arr][2, 1, 1]);
  print(mat[:, arr, :][2, 1, 1]);
  print(mat[2, :, arr][2, 1]);
  print(mat[:, 2, arr][2, 1]);
} |}
  |> Partial_evaluator.eval_prog |> print_tdata;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[4:, 2:3, 1]);
    FnPrint__(mat[2, arr[1]][1]);
    FnPrint__(mat[2, arr[1], 1]);
    FnPrint__(mat[2, 2, arr[1]]);
    FnPrint__(mat[2, 2, arr[1]]); |}]

let%expect_test "intertwined with partial evaluator" =
  Test_utils.mir_of_string
    {|
transformed data {
  vector[3] x;
  print(log(1-x[:])[:]);
} |}
  |> Partial_evaluator.eval_prog |> print_tdata;
  [%expect {|
    data vector[3] x;
    FnPrint__(log1m(x)); |}]
