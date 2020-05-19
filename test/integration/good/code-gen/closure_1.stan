data {
    int N;
    real x;
    real y[N];
    vector[N] v;
}
transformed data {
    functions
    real foo(real z, row_vector r) {
        return z + sum(r) + x + sum(y) + sum(v);
    }
}
parameters {
    real p;
    real pa[N];
    vector[N] pv;
}
model {
    functions
    real bar(real z, row_vector r) {
        return z + sum(r) + x + sum(y) + sum(v)
                + p + sum(pa) + sum(pv);
    }
    target += foo(0.0,[1.0]);
    target += foo(p,[1.0]);
    target += bar(0.0,[1.0]);
    target += bar(p,[1.0]);
}
