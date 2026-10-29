// The loop writes v9a[n] and reads the whole vector v9a with sum(v9a) in the
// same iteration. Because the read does not have subscripts, the dependence
// test cannot relate the read to the write. Direction sets are for the loop
// over n.
// Dependences: whole-vector read (no subscripts) -> confused -> sequential
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v9a = v0;
  for (n in 1 : N) {
    v9a[n] = x[n];
    target += sum(v9a);
  }
}
