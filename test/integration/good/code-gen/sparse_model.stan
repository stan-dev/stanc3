data {
  int N_dt;
  int M_dt;
  int nz_vals_dt;
  int nz_rows_dt[nz_vals_dt];
  int nz_cols_dt[nz_vals_dt];
  int N_par;
  int M_par;
  int nz_vals_par;
  int nz_rows_par[nz_vals_par];
  int nz_cols_par[nz_vals_par];
  sparse_matrix[nz_rows_dt, nz_cols_dt, N_dt, M_dt] X_dt;
}

transformed data {
  // Should be allowed
  real data_cool = X_dt[1, 1];
  // Fails correctly
  // YY_tdt[1, 1] = 10;
}


parameters {
  sparse_matrix[nz_rows_par, nz_cols_par, N_par, M_par] XX_par;
}
