functions {
  real multi_wallenius_integral(real t, // Function argument
                                real xc, array[] real theta,
                                // parameters
                                array[] real x_r,
                                // data (real)
                                array[] int x_i) {
    // data (integer)
    real Dinv = 1 / theta[1];
    int Cp1 = num_elements(x_i);
    int n = x_i[1];
    real v = 1;

    for (i in 2 : Cp1)
      v *= pow(1 - t ^ (theta[i] * Dinv), x_i[i]);

    return v;
  }

  real multi_wallenius_lpmf(data array[] int k, vector m, vector p,
                            data array[] real x_r, data real tol) {
    int C = num_elements(m);
    real D = dot_product(to_row_vector(p), (m - to_vector(k[2 : C + 1])));
    real lp = log(integrate_1d(multi_wallenius_integral, 0, 1,
                               append_array({D}, to_array_1d(p)), x_r, k,
                               tol));

    for (i in 1 : C)
      lp += -log1p(m[i]) - lbeta(m[i] - k[i + 1] + 1, k[i + 1] + 1);

    return lp;
  }
}
data {
  int<lower=0> N;
  int<lower=0> C;
  array[N, C + 1] int y;
  vector[C] m;
  real tol;
}
transformed data {
  array[0] real x_r;
  array[0] int x_i;
}
parameters {
  simplex[C] probs;
}
model {
  for (i in 1 : N)
    y[i] ~ multi_wallenius(m, probs, x_i, tol);
}
