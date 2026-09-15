// Scalar reductions: the red model for roadmap item 5 of
// design-docs/active/vectorize-loop-fission.md (§7.9, §7.9.4). A loop-carried
// scalar whose every access in the body is an increment `s += e` (MIR
// `s = s + e`, `e` free of `s`) commutes like `target +=` and becomes one
// vector statement `s += sum(e)`; a density operand reuses the `target`
// widening, an invariant operand becomes `count * e`.
// Today every increment stays sequential: the accumulator has `subs = []`,
// so its read and write give a confused self edge `{<,=,>}` and the statement
// is reported as a recurrence (only example 6 hoists anything, its plain
// assignment). The expected files pin that baseline; when item 5 lands,
// examples 1-7 must become vector statements, example 8 must split, and
// examples 9-14 must stay unchanged. The functions-block loop of example 1 is
// reported after the model-block loops.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (s3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example. Examples are ordered by the outcome
// item 5 is expected to produce: fully vectorized, then partially vectorized,
// then loops the pass must leave unchanged.
functions {
  // 1. Accumulation into a function local, the shape of most user-defined
  // densities. Runs inside the functions block; the pass visits it through
  // Program.map.
  // After item 5: lp1 += normal_lpdf(y[1:num_elements(y)] | mu[1:...], sigma)
  real normal_loop1_lpdf(vector y, vector mu, real sigma) {
    real lp1 = 0;
    for (n in 1 : num_elements(y)) {
      lp1 += normal_lpdf(y[n] | mu[n], sigma);
    }
    return lp1;
  }
}
data {
  int<lower=2> N;
  int<lower=1> K;
  vector[N] x;
  vector[N] y;
  vector[N] a;
  vector[N] b;
  vector<lower=0>[N] w;
  array[N] vector[K] rows;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  // ---- Fully vectorized once item 5 lands ----

  // 1. (see the functions block) called on a parameter vector.
  {
    vector[N] mu1 = alpha + beta * x;
    target += normal_loop1_lpdf(y | mu1, sigma);
  }

  // 2. Density accumulated into a model-block local, then added to target.
  // After item 5: lp2 += normal_lpdf(y | alpha + beta * x, sigma)
  {
    real lp2 = 0;
    for (n in 1 : N) {
      lp2 += normal_lpdf(y[n] | alpha + beta * x[n], sigma);
    }
    target += lp2;
  }

  // 3. Plain sum.
  // After item 5: total3 += sum(a[1:N])
  {
    real total3 = 0;
    for (n in 1 : N) {
      total3 += a[n];
    }
    target += total3;
  }

  // 4. Dot product written as a loop.
  // After item 5: dot4 += sum(a[1:N] .* b[1:N])  (dot_product by a later
  // partial-evaluator rule)
  {
    real dot4 = 0;
    for (n in 1 : N) {
      dot4 += a[n] * b[n];
    }
    target += dot4 * alpha;
  }

  // 5. Invariant increment: an integer counter.
  // After item 5: count5 += (N - (1 - 1))  (the iteration count)
  {
    int count5 = 0;
    for (n in 1 : N) {
      count5 += 1;
    }
    target += count5 * alpha;
  }

  // 6. Two increments of the same accumulator next to a hoistable
  // assignment; S1->S2 {Eq}, the increments carry no edge between them.
  // Today: mu6 hoists, the two increments stay in a loop (partial).
  // After item 5: mu6[1:N] = alpha + beta * x; s6 += sum(mu6[1:N]);
  //               s6 += sum(w[1:N])
  {
    vector[N] mu6;
    real s6 = 0;
    for (n in 1 : N) {
      mu6[n] = alpha + beta * x[n];
      s6 += mu6[n];
      s6 += w[n];
    }
    target += s6;
  }

  // 7. Decrement (`s -= e` is `s = s - e`).
  // After item 5: neg7 -= sum(a[1:N])
  {
    real neg7 = 0;
    for (n in 1 : N) {
      neg7 -= a[n];
    }
    target += neg7;
  }

  // ---- Partially vectorized once item 5 lands ----

  // 8. Reduction over a recurrence's output: S1 self {Lt, 1} stays a loop,
  // S1->S2 {Eq} lets S2 follow it as a vector statement.
  // After item 5: for (n in 2:N) v8[n] = v8[n - 1] + w[n];
  //               s8 += sum(v8[2:N])
  {
    vector[N] v8 = w;
    real s8 = 0;
    for (n in 2 : N) {
      v8[n] = v8[n - 1] + w[n];
      s8 += v8[n];
    }
    target += sum(v8) + s8;
  }

  // ---- Left unchanged: not a reduction ----

  // 9. The running sum is observed by another statement (the analogue of
  // reading target() in the body): every access stays ordered.
  {
    real s9 = 0;
    vector[N] z9;
    for (n in 1 : N) {
      s9 += a[n];
      z9[n] = s9;
    }
    target += sum(z9);
  }

  // 10. The operand mentions the accumulator: a genuine recurrence.
  {
    real s10 = 1;
    for (n in 1 : N) {
      s10 = s10 + s10 * a[n];
    }
    target += s10;
  }

  // 11. Increment under an if: an `IfElse` leaf (item 6), not a reduction.
  {
    real s11 = 0;
    for (n in 1 : N) {
      if (a[n] > 0) s11 += a[n];
    }
    target += s11;
  }

  // 12. Mixed operand: the density already sums while `log(w[n])` widens to
  // a vector, so `sum` of the widened operand would be wrong. Refused.
  {
    real lp12 = 0;
    for (n in 1 : N) {
      lp12 += normal_lpdf(y[n] | alpha, sigma) + log(w[n]);
    }
    target += lp12;
  }

  // 13. Vector accumulator: out of scope for item 5 (scalars only).
  {
    vector[K] acc13 = rep_vector(0, K);
    for (n in 1 : N) {
      acc13 += rows[n];
    }
    target += sum(acc13);
  }

  // 14. Product accumulator: Plus and Minus only in the first cut.
  {
    real prod14 = 1;
    for (n in 1 : N) {
      prod14 *= a[n];
    }
    target += prod14;
  }
}
