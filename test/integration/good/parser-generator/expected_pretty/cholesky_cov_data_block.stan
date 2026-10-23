/* $ stanc --auto-format ../cholesky_cov_data_block.stan : [exit 0] */
data {
  cholesky_factor_cov[5, 4] cfcov_54;
  cholesky_factor_cov[3] cfcov_33;
}

