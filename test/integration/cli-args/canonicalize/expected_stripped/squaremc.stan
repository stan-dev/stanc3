/* $ stanc --auto-format --canonicalize strip-comments,braces,parentheses,deprecations ../squaremc.stan : [exit 0] */
parameters {
  real<lower=-1, upper=1> x_raw;
  real<lower=-(1 - sqrt(1 - square(1 - abs(x_raw)))),
       upper=(1 - sqrt(1 - square(1 - abs(x_raw))))> y_raw;
}
transformed parameters {
  real<lower=-1, upper=1> x;
  real<lower=-1, upper=1> y;
  x = ((x_raw > 0) ? 1 : -1) - x_raw;
  y = ((y_raw > 0) ? 1 : -1) - y_raw;
}
model {
  target += log1m(sqrt(1 - square(1 - abs(x_raw))));
}

