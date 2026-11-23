data {
  int<lower=1, upper=10> k;
}
parameters {
  real a;
}
model {
  vector[10] stable = rep_vector(0, 10);
  vector[10] changed = rep_vector(0, 10);
  vector[10] conditional = rep_vector(0, 10);
  int i = 1;
  a ~ std_normal();

  stable[k] = a;
  stable[k] = 0;
  // No warning. Both writes select the same element.
  if (stable[k] > 0) print(stable[k]);

  changed[i] = a;
  i = 2;
  changed[i] = 0;
  // Warn. The second write leaves a in element 1.
  if (changed[1] > 0) print(changed[1]);

  conditional[1] = a;
  if (k > 1) conditional[1] = 0;
  // Warn. When k is 1, the overwrite never runs.
  if (conditional[1] > 0) print(conditional[1]);
}
