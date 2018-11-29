parameters {
#include incl_stanc_helper.stan
}
model {
  real ww = 1;
  y ~ normal(0, 1);
}
generated quantities {
  print(ww);
}
