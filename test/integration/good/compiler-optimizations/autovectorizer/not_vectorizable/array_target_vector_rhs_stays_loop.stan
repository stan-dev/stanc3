// An assignment loop `p[n] = inv_logit(logit_p[n])` where the target is a
// real array and `logit_p` is a vector. The loop assigns a real to a real,
// but the widened right-hand side is a vector.
// Desired outcome: left as a loop.
// Assigning a vector to an array slice does not typecheck.
data {
  int<lower=0> N;
}
parameters {
  vector[N] logit_p;
}
model {
  array[N] real p;
  for (n in 1 : N) {
    p[n] = inv_logit(logit_p[n]);
  }
  target += normal_lpdf(p | 0, 1);
}
