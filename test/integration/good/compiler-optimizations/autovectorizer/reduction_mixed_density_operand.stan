// Scalar reductions, roadmap item 5 of
// design-docs/active/vectorize-loop-fission.md (section 7.4, section 7.9). A
// scalar whose every access in the loop body is an increment `s += e` (MIR
// `s = s + e`, `e` free of `s`) is an accumulator: its increments commute
// like `target +=`, so they carry no dependence edge and each becomes one
// vector statement `s += sum(e over the loop)`. A density operand reuses the
// `target` widening, an invariant operand becomes `count * e`. Reals are
// summed in a different order than the loop, the reassociation Stan already
// accepts for `target`. Any other access to the scalar (a read elsewhere, an
// operand that mentions it, a declaration in the body) gives the increment a
// confused edge `{<,=,>}` that keeps the loop sequential; an increment nested
// in an `if` still commutes, but the if statement itself is not widened.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 12. Mixed operand: the density already sums while `log(w[n])` widens to
// a vector, so `sum` of the widened operand would be wrong. Refused.
data {
  int<lower=2> N;
  vector[N] y;
  vector<lower=0>[N] w;
}
parameters {
  real alpha;
  real<lower=0> sigma;
}
model {
  real lp12 = 0;
  for (n in 1 : N) {
    lp12 += normal_lpdf(y[n] | alpha, sigma) + log(w[n]);
  }
  target += lp12;
}
