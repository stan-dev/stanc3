// A print statement must stay in the loop and reads mu3[n], which the
// statement before it writes. The write is hoisted into a vector statement
// placed before the remaining loop. Direction sets are for the loop over n.
// Dependences: true a->b {Eq}
// Desired outcome: partially vectorized.
//   `mu3[:] = 2 * x` then `for (n in 1:N) print(mu3[n]);`
data {
  int<lower=2> N;
  vector[N] x;
}
model {
  vector[N] mu3;
  for (n in 1 : N) {
    mu3[n] = 2 * x[n];
    print(mu3[n]);
  }
  target += sum(mu3);
}
