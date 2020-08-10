data {
  int a0, a1, a2;
  array[5] real b0, b1, b3;
  array[3,2] vector[5] c0, c1, c2, c4;
  array[3,2] matrix[5,4] d0, d1;
}
transformed data {
  int td_a0=0, td_a1 = 2;
  int td_a2, td_a3;
  int td_a4 = 4, td_a5, td_a6 = 6;

  real td_b0=0.5, td_b1 = 2.5;
  real td_b2, td_b3;
  real td_b4 = 4.5, td_b5, td_b6 = 6.5;

  vector[2] td_c0 = [-1.5, -2.5]', td_c1 = [1.5, 2.5]';
  vector[2] td_c2, td_c3;
  vector[2] td_c4 = [-1.5, -2.5]', td_c5, td_c6 = [1.5, 2.5]';

  array[2] real td_d0 = {-1.5, -2.5}, td_d1 = {1.5, 2.5};
  array[2] real td_d2, td_d3;
  array[2] real td_d4 = {-1.5, -2.5}, td_d5, td_d6 = {1.5, 2.5};

  {
    int loc_a0=0, loc_a1 = 2;
    int loc_a2, loc_a3;
    int loc_a4 = 4, loc_a5, loc_a6 = 6;

    real loc_b0=0.5, loc_b1 = 2.5;
    real loc_b2, loc_b3;
    real loc_b4 = 4.5, loc_b5, loc_b6 = 6.5;

    vector[2] loc_c0 = [-1.5, -2.5]', loc_c1 = [1.5, 2.5]';
    vector[2] loc_c2, loc_c3;
    vector[2] loc_c4 = [-1.5, -2.5]', loc_c5, loc_c6 = [1.5, 2.5]';

    array[2] real loc_d0 = {-1.5, -2.5}, loc_d1 = {1.5, 2.5};
    array[2] real loc_d2, loc_d3;
    array[2] real loc_d4 = {-1.5, -2.5}, loc_d5, loc_d6 = {1.5, 2.5};
  }
}
parameters {
  array[5] real par_b0, par_b1, par_b3;
  array[3,2] vector[5] par_c0, par_c1, par_c2, par_c4;
  array[3,2] matrix[5,4] par_d0, par_d1;
}
transformed parameters {
  real tpar_b0=0.5, tpar_b1 = 2.5;
  real tpar_b2, tpar_b3;
  real tpar_b4 = 4.5, tpar_b5, tpar_b6 = 6.5;

  vector[2] tpar_c0 = [-1.5, -2.5]', tpar_c1 = [1.5, 2.5]';
  vector[2] tpar_c2, tpar_c3;
  vector[2] tpar_c4 = [-1.5, -2.5]', tpar_c5, tpar_c6 = [1.5, 2.5]';

  array[2] real tpar_d0 = {-1.5, -2.5}, tpar_d1 = {1.5, 2.5};
  array[2] real tpar_d2, tpar_d3;
  array[2] real tpar_d4 = {-1.5, -2.5}, tpar_d5, tpar_d6 = {1.5, 2.5};

  {
    int loc_tpar_a0=0, loc_tpar_a1 = 2;
    int loc_tpar_a2, loc_tpar_a3;
    int loc_tpar_a4 = 4, loc_tpar_a5, loc_tpar_a6 = 6;

    real loc_tpar_b0=0.5, loc_tpar_b1 = 2.5;
    real loc_tpar_b2, loc_tpar_b3;
    real loc_tpar_b4 = 4.5, loc_tpar_b5, loc_tpar_b6 = 6.5;

    vector[2] loc_tpar_c0 = [-1.5, -2.5]', loc_tpar_c1 = [1.5, 2.5]';
    vector[2] loc_tpar_c2, loc_tpar_c3;
    vector[2] loc_tpar_c4 = [-1.5, -2.5]', loc_tpar_c5, loc_tpar_c6 = [1.5, 2.5]';

    array[2] real loc_tpar_d0 = {-1.5, -2.5}, loc_tpar_d1 = {1.5, 2.5};
    array[2] real loc_tpar_d2, loc_tpar_d3;
    array[2] real loc_tpar_d4 = {-1.5, -2.5}, loc_tpar_d5, loc_tpar_d6 = {1.5, 2.5};
  }
}

model {
  int model_a0=0, model_a1 = 2;
  int model_a2, model_a3;
  int model_a4 = 4, model_a5, model_a6 = 6;

  real model_b0=0.5, model_b1 = 2.5;
  real model_b2, model_b3;
  real model_b4 = 4.5, model_b5, model_b6 = 6.5;

  vector[2] model_c0 = [-1.5, -2.5]', model_c1 = [1.5, 2.5]';
  vector[2] model_c2, model_c3;
  vector[2] model_c4 = [-1.5, -2.5]', model_c5, model_c6 = [1.5, 2.5]';

  array[2] real model_d0 = {-1.5, -2.5}, model_d1 = {1.5, 2.5};
  array[2] real model_d2, model_d3;
  array[2] real model_d4 = {-1.5, -2.5}, model_d5, model_d6 = {1.5, 2.5};

  {
    int loc_model_a0=0, loc_model_a1 = 2;
    int loc_model_a2, loc_model_a3;
    int loc_model_a4 = 4, loc_model_a5, loc_model_a6 = 6;

    real loc_model_b0=0.5, loc_model_b1 = 2.5;
    real loc_model_b2, loc_model_b3;
    real loc_model_b4 = 4.5, loc_model_b5, loc_model_b6 = 6.5;

    vector[2] loc_model_c0 = [-1.5, -2.5]', loc_model_c1 = [1.5, 2.5]';
    vector[2] loc_model_c2, loc_model_c3;
    vector[2] loc_model_c4 = [-1.5, -2.5]', loc_model_c5, loc_model_c6 = [1.5, 2.5]';

    array[2] real loc_model_d0 = {-1.5, -2.5}, loc_model_d1 = {1.5, 2.5};
    array[2] real loc_model_d2, loc_model_d3;
    array[2] real loc_model_d4 = {-1.5, -2.5}, loc_model_d5, loc_model_d6 = {1.5, 2.5};
  }

}
generated quantities {
  int gq_a0=0, gq_a1 = 2;
  int gq_a2, gq_a3;
  int gq_a4 = 4, gq_a5, gq_a6 = 6;

  real gq_b0=0.5, gq_b1 = 2.5;
  real gq_b2, gq_b3;
  real gq_b4 = 4.5, gq_b5, gq_b6 = 6.5;

  vector[2] gq_c0 = [-1.5, -2.5]', gq_c1 = [1.5, 2.5]';
  vector[2] gq_c2, gq_c3;
  vector[2] gq_c4 = [-1.5, -2.5]', gq_c5, gq_c6 = [1.5, 2.5]';

  array[2] real gq_d0 = {-1.5, -2.5}, gq_d1 = {1.5, 2.5};
  array[2] real gq_d2, gq_d3;
  array[2] real gq_d4 = {-1.5, -2.5}, gq_d5, gq_d6 = {1.5, 2.5};

  {
    int loc_gq_a0=0, loc_gq_a1 = 2;
    int loc_gq_a2, loc_gq_a3;
    int loc_gq_a4 = 4, loc_gq_a5, loc_gq_a6 = 6;

    real loc_gq_b0=0.5, loc_gq_b1 = 2.5;
    real loc_gq_b2, loc_gq_b3;
    real loc_gq_b4 = 4.5, loc_gq_b5, loc_gq_b6 = 6.5;

    vector[2] loc_gq_c0 = [-1.5, -2.5]', loc_gq_c1 = [1.5, 2.5]';
    vector[2] loc_gq_c2, loc_gq_c3;
    vector[2] loc_gq_c4 = [-1.5, -2.5]', loc_gq_c5, loc_gq_c6 = [1.5, 2.5]';

    array[2] real loc_gq_d0 = {-1.5, -2.5}, loc_gq_d1 = {1.5, 2.5};
    array[2] real loc_gq_d2, loc_gq_d3;
    array[2] real loc_gq_d4 = {-1.5, -2.5}, loc_gq_d5, loc_gq_d6 = {1.5, 2.5};
  }
}
