data {
  int N_dt;
  int M_dt;
  int nz_vals_dt;
  int nz_rows_dt[nz_vals_dt];
  int nz_cols_dt[nz_vals_dt];
  int N_tdt;
  int M_tdt;
  int nz_vals_tdt;
  int nz_rows_tdt[nz_vals_tdt];
  int nz_cols_tdt[nz_vals_tdt];
  int N_par;
  int M_par;
  int nz_vals_par;
  int nz_rows_par[nz_vals_par];
  int nz_cols_par[nz_vals_par];
  int N_tpar;
  int M_tpar;
  int nz_vals_tpar;
  int nz_rows_tpar[nz_vals_tpar];
  int nz_cols_tpar[nz_vals_tpar];
  sparse_matrix[nz_rows_dt, nz_cols_dt, N_dt, M_dt] X_dt;
}

transformed data {
  // Ban this or check X_dt * transpose(X_dt) has same dimensionality
  sparse_matrix[nz_rows_tdt, nz_cols_tdt, N_tdt, M_tdt] YY_tdt = X_dt * transpose(X_dt);
  // Should be allowed
  real data_cool = YY_tdt[1, 1];
  // Fails correctly
  // YY_tdt[1, 1] = 10;
}


parameters {
  sparse_matrix[nz_rows_par, nz_cols_par, N_par, M_par] XX_par;
}

transformed parameters {
  // Ban this or check XX_par * transpose(X_dt) has same nonzeros
  sparse_matrix[nz_rows_tpar, nz_cols_tpar, N_tpar, M_tpar] VV_tpar = XX_par * transpose(X_dt);
}

generated quantities {
  // Ban this or check XX_par * transpose(X_dt) has same nonzeros
  sparse_matrix[nz_rows_tpar, nz_cols_tpar, N_tpar, M_tpar] VV_gen = XX_par * transpose(X_dt);
}
