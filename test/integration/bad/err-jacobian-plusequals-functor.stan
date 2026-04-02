functions {
  // nonsense for testing
  vector sho_jacobian(real t, // time
                      vector y, // state
                      real theta) {
    // friction parameter
    vector[2] dydt;
    dydt[1] = y[2];
    dydt[2] = -y[1] - theta * y[2];
    jacobian += dydt;
    return dydt;
  }

  void weird_jacobian(data vector y0, data real t0, data array[] real ts,
                      data real theta) {
    jacobian += ode_rk45(sho_jacobian, y0, t0, ts, theta);
  }
}

