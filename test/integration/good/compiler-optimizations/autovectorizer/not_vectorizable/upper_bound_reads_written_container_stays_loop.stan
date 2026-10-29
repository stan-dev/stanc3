// The loop runs to `size(w)` and assigns `w[i] = a[i]`. The upper bound reads
// the container that the body writes.
// Desired outcome: left as a loop.
// The pass keeps any loop whose body writes a variable that the upper bound
// reads.
data {
  int<lower=0> N;
  vector[N] a;
}
model {
  vector[N] w;
  for (i in 1 : size(w)) {
    w[i] = a[i];
  }
  print(w);
}
