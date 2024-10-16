data {
}
model {
    // known family, known suffix, not implemented
    target += binomial_lupdf(1|0,1);
}
