functions {
    real test(real y) {
        return normal_lupdf(y|0,1);
    }
}
transformed parameters {
    real p;
    real y = test(p);
}
