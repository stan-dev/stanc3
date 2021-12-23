data {
  int a0;
  array[1] int a1;
  array[2, 3] int a2;

  real b0;
  array[4] real b1;
  array[5, 6] real b2;

  vector[7] c0;
  array[8] vector[7] c1;
  array[8, 9] vector[7] c2;

  row_vector[7] d0;
  array[8] row_vector[7] d1;
  array[8, 9] row_vector[7] d2;

  matrix[8, 9] e;
  array[11] matrix[8, 9] e1;
  array[11, 12] matrix[8, 9] e2;

  simplex[8] f;
  array[9] simplex[8] f1;
  array[9, 10] simplex[8] f2;

  ordered[9] g;
  array[11] ordered[9] g1;
  array[12, 13] ordered[9] g2;

  positive_ordered[9] h;
  array[11] positive_ordered[9] h1;
  array[12, 13] positive_ordered[9] h2;

  corr_matrix[10] j;
  array[3] corr_matrix[10] j1;
  array[5, 6] corr_matrix[10] j2;

  cov_matrix[10] k;
  array[3] cov_matrix[10] k1;
  array[5, 6] cov_matrix[10] k2;
}
transformed data {
  int td_a0;
  array[1] int td_a1;
  array[2, 3] int td_a2;

  real td_b0;
  array[4] real td_b1;
  array[5, 6] real td_b2;

  vector[7] td_c0;
  array[8] vector[7] td_c1;
  array[8, 9] vector[7] td_c2;

  row_vector[7] td_d0;
  array[8] row_vector[7] td_d1;
  array[8, 9] row_vector[7] td_d2;

  matrix[8, 9] td_e;
  array[11] matrix[8, 9] td_e1;
  array[11, 12] matrix[8, 9] td_e2;

  simplex[8] td_f;
  array[9] simplex[8] td_f1;
  array[9, 10] simplex[8] td_f2;

  ordered[9] td_g;
  array[11] ordered[9] td_g1;
  array[12, 13] ordered[9] td_g2;

  positive_ordered[9] td_h;
  array[11] positive_ordered[9] td_h1;
  array[12, 13] positive_ordered[9] td_h2;

  corr_matrix[10] td_j;
  array[3] corr_matrix[10] td_j1;
  array[5, 6] corr_matrix[10] td_j2;

  cov_matrix[10] td_k;
  array[3] cov_matrix[10] td_k1;
  array[5, 6] cov_matrix[10] td_k2;

  {
    int loc_td_a0;
    array[1] int loc_td_a1;
    array[2, 3] int loc_td_a2;

    real loc_td_b0;
    array[4] real loc_td_b1;
    array[5, 6] real loc_td_b2;

    vector[7] loc_td_c0;
    array[8] vector[7] loc_td_c1;
    array[8, 9] vector[7] loc_td_c2;

    row_vector[7] loc_td_d0;
    array[8] row_vector[7] loc_td_d1;
    array[8, 9] row_vector[7] loc_td_d2;

    matrix[8, 9] loc_td_e;
    array[11] matrix[8, 9] loc_td_e1;
    array[11, 12] matrix[8, 9] loc_td_e2;
  }
}
parameters {
  real par_b0;
  array[4] real par_b1;
  array[5, 6] real par_b2;

  vector[7] par_c0;
  array[8] vector[7] par_c1;
  array[8, 9] vector[7] par_c2;

  row_vector[7] par_d0;
  array[8] row_vector[7] par_d1;
  array[8, 9] row_vector[7] par_d2;

  matrix[8, 9] par_e;
  array[11] matrix[8, 9] par_e1;
  array[11, 12] matrix[8, 9] par_e2;

  simplex[8] par_f;
  array[9] simplex[8] par_f1;
  array[9, 10] simplex[8] par_f2;

  ordered[9] par_g;
  array[11] ordered[9] par_g1;
  array[12, 13] ordered[9] par_g2;

  positive_ordered[9] par_h;
  array[11] positive_ordered[9] par_h1;
  array[12, 13] positive_ordered[9] par_h2;

  corr_matrix[10] par_j;
  array[3] corr_matrix[10] par_j1;
  array[5, 6] corr_matrix[10] par_j2;

  cov_matrix[10] par_k;
  array[3] cov_matrix[10] par_k1;
  array[5, 6] cov_matrix[10] par_k2;
}
transformed parameters {
  real tpar_b0;
  array[4] real tpar_b1;
  array[5, 6] real tpar_b2;

  vector[7] tpar_c0;
  array[8] vector[7] tpar_c1;
  array[8, 9] vector[7] tpar_c2;

  row_vector[7] tpar_d0;
  array[8] row_vector[7] tpar_d1;
  array[8, 9] row_vector[7] tpar_d2;

  matrix[8, 9] tpar_e;
  array[11] matrix[8, 9] tpar_e1;
  array[11, 12] matrix[8, 9] tpar_e2;

  simplex[8] tpar_f;
  array[9] simplex[8] tpar_f1;
  array[9, 10] simplex[8] tpar_f2;

  ordered[9] tpar_g;
  array[11] ordered[9] tpar_g1;
  array[12, 13] ordered[9] tpar_g2;

  positive_ordered[9] tpar_h;
  array[11] positive_ordered[9] tpar_h1;
  array[12, 13] positive_ordered[9] tpar_h2;

  corr_matrix[10] tpar_j;
  array[3] corr_matrix[10] tpar_j1;
  array[5, 6] corr_matrix[10] tpar_j2;

  cov_matrix[10] tpar_k;
  array[3] cov_matrix[10] tpar_k1;
  array[5, 6] cov_matrix[10] tpar_k2;

  {
    int loc_tpar_a0;
    array[1] int loc_tpar_a1;
    array[2, 3] int loc_tpar_a2;

    real loc_tpar_b0;
    array[4] real loc_tpar_b1;
    array[5, 6] real loc_tpar_b2;

    vector[7] loc_tpar_c0;
    array[8] vector[7] loc_tpar_c1;
    array[8, 9] vector[7] loc_tpar_c2;

    row_vector[7] loc_tpar_d0;
    array[8] row_vector[7] loc_tpar_d1;
    array[8, 9] row_vector[7] loc_tpar_d2;

    matrix[8, 9] loc_tpar_e;
    array[11] matrix[8, 9] loc_tpar_e1;
    array[11, 12] matrix[8, 9] loc_tpar_e2;
  }
}
model {
  {
    int loc_model_a0;
    array[1] int loc_model_a1;
    array[2, 3] int loc_model_a2;

    real loc_model_b0;
    array[4] real loc_model_b1;
    array[5, 6] real loc_model_b2;

    vector[7] loc_model_c0;
    array[8] vector[7] loc_model_c1;
    array[8, 9] vector[7] loc_model_c2;

    row_vector[7] loc_model_d0;
    array[8] row_vector[7] loc_model_d1;
    array[8, 9] row_vector[7] loc_model_d2;

    matrix[8, 9] loc_model_e;
    array[11] matrix[8, 9] loc_model_e1;
    array[11, 12] matrix[8, 9] loc_model_e2;
  }
}
generated quantities {
  real gq_b0;
  array[4] real gq_b1;
  array[5, 6] real gq_b2;

  vector[7] gq_c0;
  array[8] vector[7] gq_c1;
  array[8, 9] vector[7] gq_c2;

  row_vector[7] gq_d0;
  array[8] row_vector[7] gq_d1;
  array[8, 9] row_vector[7] gq_d2;

  matrix[8, 9] gq_e;
  array[11] matrix[8, 9] gq_e1;
  array[11, 12] matrix[8, 9] gq_e2;

  simplex[8] gq_f;
  array[9] simplex[8] gq_f1;
  array[9, 10] simplex[8] gq_f2;

  ordered[9] gq_g;
  array[11] ordered[9] gq_g1;
  array[12, 13] ordered[9] gq_g2;

  positive_ordered[9] gq_h;
  array[11] positive_ordered[9] gq_h1;
  array[12, 13] positive_ordered[9] gq_h2;

  corr_matrix[10] gq_j;
  array[3] corr_matrix[10] gq_j1;
  array[5, 6] corr_matrix[10] gq_j2;

  cov_matrix[10] gq_k;
  array[3] cov_matrix[10] gq_k1;
  array[5, 6] cov_matrix[10] gq_k2;

  {
    int loc_gq_a0;
    array[1] int loc_gq_a1;
    array[2, 3] int loc_gq_a2;

    real loc_gq_b0;
    array[4] real loc_gq_b1;
    array[5, 6] real loc_gq_b2;

    vector[7] loc_gq_c0;
    array[8] vector[7] loc_gq_c1;
    array[8, 9] vector[7] loc_gq_c2;

    row_vector[7] loc_gq_d0;
    array[8] row_vector[7] loc_gq_d1;
    array[8, 9] row_vector[7] loc_gq_d2;

    matrix[8, 9] loc_gq_e;
    array[11] matrix[8, 9] loc_gq_e1;
    array[11, 12] matrix[8, 9] loc_gq_e2;
  }
}

