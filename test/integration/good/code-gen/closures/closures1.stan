data {
    int N;
    real x;
    real y[N];
    vector[N] v;
}
transformed data {
    function
    real foo(real z, row_vector r) {
        real rs = sum(r);
        real ys = sum(y);
        real vs = sum(v);
        return z + rs + x + ys + vs;
    }
}
parameters {
    real p;
    real pa[N];
    vector[N] pv;
}
model {
    function
    real bar(real z, row_vector r) {
        real rs = sum(r);
        real ys = sum(y);
        real vs = sum(v);
        real pas = sum(pa);
        real pvs = sum(pv);
        return z + rs + x + ys + vs + p + pas + pvs;
    }
    target += foo(0.0,[1.0]);
    target += foo(p,[1.0]);
    target += bar(0.0,[1.0]);
    target += bar(p,[1.0]);
}