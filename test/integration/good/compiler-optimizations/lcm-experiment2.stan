parameters {
  real x;
}
model {
  real y = log(x);
  for (j in 1:1) {
    y += 1;
  }
  if (1 > 0)
    y += 1;
  target += y;
}
