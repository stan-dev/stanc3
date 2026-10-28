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
// 13. Vector accumulator: out of scope for item 5 (scalars only).
data {
  int<lower=2> N;
  int<lower=1> K;
  array[N] vector[K] rows;
}
model {
  vector[K] acc13 = rep_vector(0, K);
  for (n in 1 : N) {
    acc13 += rows[n];
  }
  target += sum(acc13);
}
