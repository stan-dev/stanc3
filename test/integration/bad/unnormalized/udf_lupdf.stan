functions {
    real foo_lupdf(real y) {
        return 1.0;
    }
}
parameters {
    real y;
}
model {
    y ~ std_normal();
}