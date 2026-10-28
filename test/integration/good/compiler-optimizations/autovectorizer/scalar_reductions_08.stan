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
// Outcome on feat/loop-vectorization-dependence: Partially vectorized
// 8. Reduction over a recurrence's output: S1 self {Lt, 1} stays a loop,
// S1->S2 {Eq} lets S2 follow it as a vector statement.
// Emitted: for (n in 2:N) v8[n] = v8[n - 1] + w[n];
//          s8 += sum(v8[2:N])
data {
  int<lower=2> N;
  vector<lower=0>[N] w;
}
model {
  vector[N] v8 = w;
  real s8 = 0;
  for (n in 2 : N) {
    v8[n] = v8[n - 1] + w[n];
    s8 += v8[n];
  }
  target += sum(v8) + s8;
}
