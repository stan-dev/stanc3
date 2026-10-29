// The loop body holds `while (1) break;` and then `b[i] = exp(x[i])`. The
// `break` leaves only the inner while loop, so the assignment can still be
// widened. The assignment moves out in front of the loop and the loop keeps
// only the while loop.
// Desired outcome: partially vectorized.
//   `b[1:N] = exp(x[1:N])`
data {
  int<lower=0> N;
  vector[N] x;
}
model {
  vector[N] b;
  for (i in 1 : N) {
    while (1) break;
    b[i] = exp(x[i]);
  }
}
