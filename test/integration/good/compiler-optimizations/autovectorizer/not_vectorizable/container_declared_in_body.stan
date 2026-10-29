// A vector t9c is declared inside the loop body, then written and read. A
// container declared in the body does not get loop subscripts. The dependence
// test then cannot separate one iteration's accesses from another's. Direction
// sets are for the loop over n.
// Dependences: subs = [] -> confused -> sequential
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N] x;
}
model {
  vector[N] y9c;
  for (n in 1 : N) {
    vector[2] t9c;
    t9c[1] = x[n];
    y9c[n] = t9c[1];
  }
  target += sum(y9c);
}
