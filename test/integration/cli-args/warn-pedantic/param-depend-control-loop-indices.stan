parameters {
  real a;
}
model {
  vector[10] shifted = rep_vector(0, 10);
  vector[10] previous = rep_vector(0, 10);
  a ~ std_normal();

  for (n in 1:5) {
    shifted[n + 5] = a;
    // Warn. The analysis cannot prove the two ranges disjoint from the loop bounds.
    if (shifted[n] > 0) print(shifted[n]);
  }

  for (n in 2:5) {
    // Warn. The previous iteration can supply this element.
    if (previous[n - 1] > 0) print(previous[n - 1]);
    previous[n] = a;
  }
}
