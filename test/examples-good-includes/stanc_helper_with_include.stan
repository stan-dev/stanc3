parameters {
real z;
#include incl_stanc_helper.stan
}
model {
  w ~ normal(0, 1);
}
