functions {
  real yield(real b) {
    return b*3;
  }
  real func(real await) {
    return await+1;
  }
}
data {
    int lambda;
}
transformed data {
   real d = lambda / 3;
}
parameters {
   real finally;
}
transformed parameters {
   real assert = finally + 2;
}
model {
   target += normal_lpdf(d | yield(assert),1);
   target += binomial_logit_lpmf(lambda | 10, func(finally));
}