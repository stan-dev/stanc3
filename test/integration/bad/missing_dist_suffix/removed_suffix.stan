data {
}
model {
    target += binomial_logit_ccdf_log(1, 0,1);
}