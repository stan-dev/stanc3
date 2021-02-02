functions {
    void foo_lp(real y1, real y2) {
        target += normal_lupdf(y1| y2, y2);
    }
}
parameters {
    real y1;
    real y2;
}
transformed parameters {
   foo_lp(y1, y2);
}