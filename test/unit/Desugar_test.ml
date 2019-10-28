open Core_kernel
open Analysis_and_optimization

let to_mir s =
  let ast, warnings = Frontend.Frontend_utils.typed_ast_of_string_exn s in
  (Frontend.Ast_to_Mir.trans_prog "test prog" ast, warnings)

let print_tdata Middle.Program.({prepare_data; _}) =
  Fmt.(strf "@[<v>%a@]@," (list ~sep:cut Middle.Stmt.Located.pp) prepare_data)
  |> print_endline

let%expect_test "matrix array multi indexing " =
  let mir, warnings =
    to_mir
      {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat[5];
  print(mat[2, arr, 2]);
} |}
  in
  print_endline warnings ;
  [%expect {| |}] ;
  Partial_evaluator.eval_prog mir |> print_tdata ;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[2, arr, 2]); |}]

let%expect_test "matrix array multi indexing " =
  let mir, warnings =
    to_mir
      {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat[5];
  print(mat[2][arr][2]);
} |}
  in
  print_endline warnings ;
  [%expect {| |}] ;
  Partial_evaluator.eval_prog mir |> print_tdata ;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[2][arr[2]]);
 |}]

let%expect_test "matrix array multi indexing " =
  let mir, warnings =
    to_mir
      {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat[5];
  print(mat[2, arr, arr][2, 2]);
} |}
  in
  print_endline warnings ;
  [%expect {| |}] ;
  Partial_evaluator.eval_prog mir |> print_tdata ;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[2, arr[2], arr[2]]);
 |}]

let%expect_test "matrix array multi indexing " =
  let mir, warnings =
    to_mir
      {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat[5];
  print(mat[3:, 2:3][2, 1]);
} |}
  in
  print_endline warnings ;
  [%expect {| |}] ;
  mir |> Partial_evaluator.eval_prog |> print_tdata ;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[4, 2]);
 |}]

let%expect_test "matrix array multi indexing " =
  let mir, warnings =
    to_mir
      {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3,4] mat[5];
  print(mat[:3, 1, :]);
} |}
  in
  print_endline warnings ;
  [%expect {| |}] ;
  mir |> Partial_evaluator.eval_prog |> print_tdata ;
  [%expect
    {|
    data array[int, 3] arr;
    arr = FnMakeArray__(2, 3, 1);
    data array[matrix[3, 4], 5] mat;
    FnPrint__(mat[1:3, 1]);
 |}]

let%expect_test "matrix array multi indexing " =
  let mir, warnings =
    to_mir
      {|
transformed data {
  int arr[3] = {2, 3, 1};
  matrix[3, 4] mat[5];
  print(mat[4:, 2:3, 1]);
  print(mat[:, arr][2, 1, 1]);
  print(mat[:, arr, :][2, 1, 1]);
  print(mat[2, :, arr][2, 1]);
  print(mat[:, 2, arr][2, 1]);
} |}
  in
  print_endline warnings ;
  [%expect {| |}] ;
  mir |> Partial_evaluator.eval_prog |> print_tdata ;
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
  let mir, warnings =
    to_mir {|
transformed data {
  vector[3] x;
  print(log(1-x[:])[:]);
} |}
  in
  print_endline warnings ;
  [%expect {| |}] ;
  mir |> Partial_evaluator.eval_prog |> print_tdata ;
  [%expect {|
    data vector[3] x;
    FnPrint__(log1m(x)); |}]
