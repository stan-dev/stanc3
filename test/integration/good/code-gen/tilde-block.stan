data {
    int t;
}
parameters {
    real<lower=0> x;
}
model {
    if (t)
        x ~ student_t(10, 0, 1) T[0,];
    else
        x ~ normal(0, 1) T[0,];
}