// The design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1, one loop per example. Direction sets are for the loop over
// n; V marks a statement that becomes a vector statement, S one that stays
// in a sequential loop. Each example sits in its own block, and every local
// it writes carries the example number as a suffix (v5 belongs to example 5)
// so the generated MIR and C++ can be matched to the example.
data {
  int<lower=2> N;
  int<lower=1> J;
  int<lower=0> k;
  array[N] int<lower=1, upper=J> county_idx;
  array[N] int<lower=1, upper=N> idx;
  vector[N] log_uppm;
  vector[N] floor_measure;
  vector[N] log_radon;
  vector[N] x;
  vector[N] u;
  vector[N] v0;
  vector[N] a0;
}
parameters {
  vector[J] alpha;
  vector[2] beta;
  real<lower=0> sigma_y;
}
model {
  // 1. Radon: every dependence is {Eq}. Three vector statements, no loop.
  // edges: a->b {Eq}, b->c {Eq}. alpha, county_idx, log_uppm, ... are
  // read-only: no edges.
  // => muj1[:] = alpha[county_idx] + log_uppm * beta[1];
  //    mu1[:]  = muj1 + floor_measure * beta[2];
  //    target += normal_lpdf(log_radon | mu1, sigma_y);
  {
    vector[N] mu1;
    vector[N] muj1;
    for (n in 1 : N) {
      muj1[n] = alpha[county_idx[n]] + log_uppm[n] * beta[1]; // V
      mu1[n] = muj1[n] + floor_measure[n] * beta[2]; // V
      target += normal_lpdf(log_radon[n] | mu1[n], sigma_y); // V
    }
  }

  // 2. Anti-dependence, distance 0: order kept (Allen and Kennedy p. 508).
  // a->b {Eq}
  // => mu2[:] = v2 + 1;  v2[:] = x;
  {
    vector[N] mu2;
    vector[N] v2 = v0;
    for (n in 1 : N) {
      mu2[n] = v2[n] + 1;
      v2[n] = x[n];
    }
    target += sum(mu2) + sum(v2);
  }

  // 3. Sequential statement reads a hoisted write: a->b {Eq}, loop after.
  // => mu3[:] = 2 * x;  for (n in 1:N) print(mu3[n]);
  {
    vector[N] mu3;
    for (n in 1 : N) {
      mu3[n] = 2 * x[n];
      print(mu3[n]);
    }
    target += sum(mu3);
  }

  // 4. Sequential statement reads before the write: a->b {Eq}, loop before.
  // => for (n in 1:N) print(v4[n]);  v4[:] = x;
  {
    vector[N] v4 = v0;
    for (n in 1 : N) {
      print(v4[n]);
      v4[n] = x[n];
    }
    target += sum(v4);
  }

  // 5. Both directions through effects: a<->c (effects), a->b, b->c: one
  // strongly connected component.
  // => unchanged
  {
    vector[N] v5 = v0;
    for (n in 1 : N) {
      print(v5[n]);
      v5[n] = x[n];
      print(v5[n]);
    }
    target += sum(v5);
  }

  // 6. Recurrence: self-edge with {Lt}, distance 1. Sequential.
  // => unchanged
  {
    vector[N] v6 = v0;
    for (n in 2 : N) {
      v6[n] = v6[n - 1] + u[n];
    }
    target += sum(v6);
  }

  // 7. Carried but acyclic: a->b {Lt}, distance 1. Legal to distribute, and
  // offset widening (section 7.6) expresses it.
  // => v7[2:N] = x[2:N];  y7[2:N] = v7[1:(N-1)];   (order forced by a->b)
  {
    vector[N] v7 = v0;
    vector[N] y7 = v0;
    for (n in 2 : N) {
      v7[n] = x[n];
      y7[n] = v7[n - 1];
    }
    target += sum(v7) + sum(y7);
  }

  // 8. Scalar temporary: t8 has subs = [], so a<->b {Lt,Eq,Gt}: one SCC.
  // => unchanged (until scalar expansion, section 7.9 item 2)
  {
    vector[N] mu8;
    for (n in 1 : N) {
      real t8 = 2 * x[n];
      mu8[n] = t8 + 1;
    }
    target += sum(mu8);
  }

  // 9a. Whole-vector read of a written variable: subs = [] -> confused ->
  // sequential.
  {
    vector[N] v9a = v0;
    for (n in 1 : N) {
      v9a[n] = x[n];
      target += sum(v9a);
    }
  }

  // 9b. Gather on a written variable: Varying -> confused -> sequential.
  {
    vector[N] v9b = v0;
    vector[N] y9b;
    for (n in 1 : N) {
      v9b[idx[n]] = x[n];
      y9b[n] = v9b[idx[n]];
    }
    target += sum(v9b) + sum(y9b);
  }

  // 9c. Container declared in the body: subs = [] -> confused -> sequential.
  {
    vector[N] y9c;
    for (n in 1 : N) {
      vector[2] t9c;
      t9c[1] = x[n];
      y9c[n] = t9c[1];
    }
    target += sum(y9c);
  }

  // 10. Distinct literal positions: Independent, both hoist.
  // => v10[:, 1] = x;  w10[:] = v10[:, 2];
  {
    matrix[N, 2] v10 = rep_matrix(0, N, 2);
    vector[N] w10;
    for (n in 1 : N) {
      v10[n, 1] = x[n];
      w10[n] = v10[n, 2];
    }
    target += sum(v10) + sum(w10);
  }

  // 11. Self anti-dependence: read of a later iteration's element. No edge.
  // => a11[1:(N-1)] = a11[2:N] + 1;   (deep_copy on the right-hand side makes
  //    it safe, section 7.11)
  {
    vector[N] a11 = a0;
    for (n in 1 : (N - 1)) {
      a11[n] = a11[n + 1] + 1;
    }
    target += sum(a11);
  }

  // 12. Symbolic offset (k data): both subscripts are Affine {1; {0; [k]}},
  // the symbols cancel, a->b {Eq}. Widening shifts the range by k.
  // => v12[(1 + k):(N + k)] = x;  y12[:] = v12[(1 + k):(N + k)] * 2;
  {
    vector[N + k] v12 = rep_vector(0, N + k);
    vector[N] y12;
    for (n in 1 : N) {
      v12[n + k] = x[n];
      y12[n] = v12[n + k] * 2;
    }
    target += sum(v12) + sum(y12);
  }
}
