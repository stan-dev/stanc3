data {
  matrix[3, 3] A;
}
parameters {
  real x;
}
model {
  x ~ normal(0, 1);
  if (x < 0)
    reject("This is a test of the reject statement. Here is A:", A);
  else
    fatal_error("This is a test of the exit statement. Here is A:", A);
}
