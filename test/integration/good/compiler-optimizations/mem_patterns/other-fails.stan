data {
  int N;
}
transformed parameters {
  vector[N] one;
  vector[N] two;
  real cond;

  vector[N] three;
  vector[N] four;
  four = three .^ 2.0;

  vector[N] five;
  vector[N] six;
  six = pow(five, 2.0);
}
model {
  int dims;
  dims = 1 || 2 && size(cond > 0 ? one : two);

  tuple(vector[N], real) res;
  res.1 = three;
}
