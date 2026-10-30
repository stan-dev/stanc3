// A scalar temporary t8 is written and then read in every iteration. t8 does
// not have subscripts. Accesses to t8 therefore depend in every direction and
// both statements form one strongly connected component. Direction sets are
// for the loop over n.
// Dependences: a<->b {Lt,Eq,Gt}
// Desired outcome: left as a loop.
// Vectorizing this loop would need scalar expansion of t8.
data {
  int<lower=2> N;
  vector[N] x;
}
model {
  vector[N] mu8;
  for (n in 1 : N) {
    real t8 = 2 * x[n];
    mu8[n] = t8 + 1;
  }
  target += sum(mu8);
}
