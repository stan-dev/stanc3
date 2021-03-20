functions {
    vector[] integrate(vector[] k, vector init, real[] ts) {
        function
        vector harmonic(real t, vector y) {
            return -k[1] .* y;
        }
        return ode_rk45(harmonic, init, 0, ts);
    }
}
data {
    real ts[5];
}
parameters {
    vector[2] k[1];
    vector[2] init;
}
transformed parameters {
    vector[2] y[5] = integrate(k, init, ts);
}