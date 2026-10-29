// Inside a function, the loop assigns `draws[n] = x[n] + normal_rng(0, 1)`. A
// vector statement would draw one random number instead of one per element.
// Desired outcome: left as a loop.
// An rng call cannot be duplicated or merged. The assignment stays in the
// loop.
functions {
  real draw_sum_rng(vector x) {
    vector[num_elements(x)] draws;
    for (n in 1 : num_elements(x)) {
      draws[n] = x[n] + normal_rng(0, 1);
    }
    return sum(draws);
  }
}
model {
}
