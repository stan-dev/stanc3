functions {
  real func(real b) {
    if(b > 0.0) {
      return(1.0);
    } else {
      return(0.0);
    }
  }
}
data {
  int N;
  array[N] real x;
}
parameters {
  real<lower = 0.0> sigma;
}
model {
  x ~ normal(0, func(sigma));
}
