// Two print statements surround a write of v5[n]. The prints are ordered
// against each other through their effects. The write must stay between the
// two prints. All three statements therefore form one strongly connected
// component.
// Direction sets are for the loop over n.
// Dependences: a<->c (effects), a->b, b->c
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v5 = v0;
  for (n in 1 : N) {
    print(v5[n]);
    v5[n] = x[n];
    print(v5[n]);
  }
  target += sum(v5);
}
