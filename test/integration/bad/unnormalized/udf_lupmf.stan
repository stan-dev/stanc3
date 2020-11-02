functions {
    real foo_lupmf(real y) {
        return 1.0;
    }
}
parameters {
    real y;
}
model {
    y ~ std_normal();
}