// The design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1, one loop per example. Direction sets are for the loop over
// n; V marks a statement that becomes a vector statement, S one that stays
// in a sequential loop. Each example sits in its own block so the doc's
// variable names can be reused.
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
  // => muj[:] = alpha[county_idx] + log_uppm * beta[1];
  //    mu[:]  = muj + floor_measure * beta[2];
  //    target += normal_lpdf(log_radon | mu, sigma_y);
  {
    vector[N] mu;
    vector[N] muj;
    for (n in 1 : N) {
      muj[n] = alpha[county_idx[n]] + log_uppm[n] * beta[1]; // V
      mu[n] = muj[n] + floor_measure[n] * beta[2]; // V
      target += normal_lpdf(log_radon[n] | mu[n], sigma_y); // V
    }
  }

  // 2. Anti-dependence, distance 0: order kept (Allen and Kennedy p. 508).
  // a->b {Eq}
  // => mu[:] = v + 1;  v[:] = x;
  {
    vector[N] mu;
    vector[N] v = v0;
    for (n in 1 : N) {
      mu[n] = v[n] + 1;
      v[n] = x[n];
    }
    target += sum(mu) + sum(v);
  }

  // 3. Sequential statement reads a hoisted write: a->b {Eq}, loop after.
  // => mu[:] = 2 * x;  for (n in 1:N) print(mu[n]);
  {
    vector[N] mu;
    for (n in 1 : N) {
      mu[n] = 2 * x[n];
      print(mu[n]);
    }
    target += sum(mu);
  }

  // 4. Sequential statement reads before the write: a->b {Eq}, loop before.
  // => for (n in 1:N) print(v[n]);  v[:] = x;
  {
    vector[N] v = v0;
    for (n in 1 : N) {
      print(v[n]);
      v[n] = x[n];
    }
    target += sum(v);
  }

  // 5. Both directions through effects: a<->c (effects), a->b, b->c: one
  // strongly connected component.
  // => unchanged
  {
    vector[N] v = v0;
    for (n in 1 : N) {
      print(v[n]);
      v[n] = x[n];
      print(v[n]);
    }
    target += sum(v);
  }

  // 6. Recurrence: self-edge with {Lt}, distance 1. Sequential.
  // => unchanged
  {
    vector[N] v = v0;
    for (n in 2 : N) {
      v[n] = v[n - 1] + u[n];
    }
    target += sum(v);
  }

  // 7. Carried but acyclic: a->b {Lt}, distance 1. Legal to distribute, and
  // offset widening (section 7.6) expresses it.
  // => v[2:N] = x[2:N];  y[2:N] = v[1:(N-1)];   (order forced by a->b)
  {
    vector[N] v = v0;
    vector[N] y = v0;
    for (n in 2 : N) {
      v[n] = x[n];
      y[n] = v[n - 1];
    }
    target += sum(v) + sum(y);
  }

  // 8. Scalar temporary: t has subs = [], so a<->b {Lt,Eq,Gt}: one SCC.
  // => unchanged (until scalar expansion, section 7.9 item 2)
  {
    vector[N] mu;
    for (n in 1 : N) {
      real t = 2 * x[n];
      mu[n] = t + 1;
    }
    target += sum(mu);
  }

  // 9a. Whole-vector read of a written variable: subs = [] -> confused ->
  // sequential.
  {
    vector[N] v = v0;
    for (n in 1 : N) {
      v[n] = x[n];
      target += sum(v);
    }
  }

  // 9b. Gather on a written variable: Varying -> confused -> sequential.
  {
    vector[N] v = v0;
    vector[N] y;
    for (n in 1 : N) {
      v[idx[n]] = x[n];
      y[n] = v[idx[n]];
    }
    target += sum(v) + sum(y);
  }

  // 9c. Container declared in the body: subs = [] -> confused -> sequential.
  {
    vector[N] y;
    for (n in 1 : N) {
      vector[2] t;
      t[1] = x[n];
      y[n] = t[1];
    }
    target += sum(y);
  }

  // 10. Distinct literal positions: Independent, both hoist.
  // => v[:, 1] = x;  w[:] = v[:, 2];
  {
    matrix[N, 2] v = rep_matrix(0, N, 2);
    vector[N] w;
    for (n in 1 : N) {
      v[n, 1] = x[n];
      w[n] = v[n, 2];
    }
    target += sum(v) + sum(w);
  }

  // 11. Self anti-dependence: read of a later iteration's element. No edge.
  // => a[1:(N-1)] = a[2:N] + 1;   (deep_copy on the right-hand side makes it
  //    safe, section 7.11)
  {
    vector[N] a = a0;
    for (n in 1 : (N - 1)) {
      a[n] = a[n + 1] + 1;
    }
    target += sum(a);
  }

  // 12. Symbolic offset (k data): both subscripts are Affine {1; {0; [k]}},
  // the symbols cancel, a->b {Eq}. Widening shifts the range by k.
  // => v[(1 + k):(N + k)] = x;  y[:] = v[(1 + k):(N + k)] * 2;
  {
    vector[N + k] v = rep_vector(0, N + k);
    vector[N] y;
    for (n in 1 : N) {
      v[n + k] = x[n];
      y[n] = v[n + k] * 2;
    }
    target += sum(v) + sum(y);
  }
}
