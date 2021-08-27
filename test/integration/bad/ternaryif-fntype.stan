functions {
  vector gflow(real t, vector y){
    return y;
  }
  vector rflow(real t, vector y){
    return y;
  }
}
generated quantities {
    real t0 = 1;
    vector[1] true_states[1];
    true_states = ode_rk45(
      t0 < 0 ? rflow : gflow, [1]', t0, {1.}
    );
}