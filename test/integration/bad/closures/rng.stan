model {
    functions
    real foo_rng(real x, real y) {
        return normal_rng(x|y,1);
    }
}