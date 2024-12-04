functions {
  real bernoulli_lpmf(int y, real theta, real alpha) {
    return log(theta);
  }

  void bar_lp(int y, vector t){
    y ~ bernoulli(t);
  }
}
data {
  int y;
  int N;
}
parameters {
  vector<lower=0, upper=1>[N] theta;
}
model {
  bar_lp(y, theta);
}
