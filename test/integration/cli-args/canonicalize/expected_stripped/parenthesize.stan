/* $ stanc --auto-format --canonicalize strip-comments,braces,parentheses,deprecations ../parenthesize.stan : [exit 0] */
transformed data {
  int N = 12;
  real b = 1.5;
  
  tuple(real, tuple(real, int)) tpl = (3.1, (4.5, 2));
}
parameters {
  real<lower=((b > 3) ? 1.0 : b - 2)> x;
  matrix[N, N] m;
}
model {
  matrix[N - 3, 4] n;
  row_vector[N] v = ((1.0 + m)')[1];
  
  v = [1, 2, 3];
  for (i in 1 : (N - 3)) {
    n[i] = m[i : (i + 3), i]';
  }
  if ((b < x) && (!(x < 1))) {
    x + 4 ~ normal(0, 1 + 1) T[0, 8];
  }
  
  real y_raw;
  real y;
  y = ((y_raw > 0) ? 1 : -1) - y_raw;
  y = ((y_raw > 0) ? 1 : -1) - y_raw;
  
  int bool = ((1 < 2) > 3) < 4;
  
  tuple(real, real) t = (3, 4);
  t.2 = t.1;
  t.2 = (3, 4).1;
}

/*
warning: Found chained comparison.
    ┌─ ../parenthesize.stan:27:14
 26 │  
 27 │    int bool = 1 < 2 > 3 < 4;
    │               ^^^^^^^^^ This is interpreted as (1 < 2) > 3.
    │                         Consider if the intended meaning was (1 < 2) && (2 > 3) instead.
 28 │  
    = You can silence this warning by adding explicit
      parentheses.
    = This can be automatically changed using the canonicalize flag for
      stanc.
warning: Found chained comparison.
    ┌─ ../parenthesize.stan:27:14
 26 │  
 27 │    int bool = 1 < 2 > 3 < 4;
    │               ^^^^^^^^^^^^^ This is interpreted as (1 < 2 > 3) < 4.
    │                             Consider if the intended meaning was (1 < 2 > 3) && (3 < 4) instead.
 28 │  
    = You can silence this warning by adding explicit
      parentheses.
    = This can be automatically changed using the canonicalize flag for
      stanc.
*/
