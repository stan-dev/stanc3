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
      sparse_matrix[3, 4] mat;
    }
    |}
  in
  (try let _ = typed_ast_of_string_exn prog in ()
   with
   | Failure e -> print_endline e);
  [%expect
    {|
    Incorrect sparse_matrix declaration in the Data block; nonzero element location arrays are required. |}]

let%expect_test "invalid StaticSparseMatrix assignment" =
  let prog =
    {|
data {
  int N;
  int M;
  int nz_vals;
  int nz_rows[nz_vals];
  int nz_cols[nz_vals];
  sparse_matrix[nz_rows, nz_cols, N, M] x;
}

transformed data {
  sparse_matrix[nz_rows, nz_cols, N, M] xx = x * transpose(x);
  // Should be allowed
  real data_cool = xx[1, 1]; // (2)
  // Should not be allowed
  xx[1, 1] = 5.0; // (3)
  xx[1:10, 1:10] = x[1:10, 1:10]; // (3)
  xx = x * x;

}

parameters {
  sparse_matrix[nz_rows, nz_cols, N, M] VV;
}

transformed parameters {
  sparse_matrix[nz_rows, nz_cols, N, M] ZZ = VV * nz_vals;
  // Cool and fine
  real param_cool = ZZ[1, 1]; // (2)
  // Neither are cool or fine
  ZZ[1, 1] = 10.0; // (3)
  ZZ[1:10, 1:10] = VV[1:10, 1:10] * 10.0; // (3)
  ZZ = VV * x;
}
    |}
  in
  (try let _ = typed_ast_of_string_exn prog in ()
   with
   | Failure e -> print_endline e);
  [%expect
    {|
    Cannot assign to sparse matrix 'xx'.Cannot assign to sparse matrix 'xx'.
    Cannot assign to sparse matrix 'ZZ'.
    Cannot assign to sparse matrix 'ZZ'. |}]

(* NOTE: This check will fail when the sparsematrix is inside another container. Like if arr were an array of sparse matrices, arr[i][1,1] wouldn't throw *)

let%expect_test "invalid StaticSparseMatrix assignment within array" =
  let prog =
    {|
data {
  int N;
  int M;
  int nz_vals;
  int nz_rows[nz_vals];
  int nz_cols[nz_vals];
  sparse_matrix[nz_rows, nz_cols, N, M] x;
}

transformed data {
  sparse_matrix[N, M] xx[3];
  xx[1] = x * transpose(x);
  // Should be allowed
  real data_cool = xx[1][1, 1]; // (2)
  // Should not be allowed
  xx[1][1, 1] = 5.0; // (3)
  xx[1][1:10, 1:10] = x[1:10, 1:10]; // (3)
}
    |}
  in
  (try let _ = typed_ast_of_string_exn prog in ()
   with
   | Failure e -> print_endline e);
  [%expect
    {|
      Cannot assign to sparse matrix 'xx'.
      Cannot assign to sparse matrix 'xx'. |}]
