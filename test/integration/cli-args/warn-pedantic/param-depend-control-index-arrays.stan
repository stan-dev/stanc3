data {
  int<lower=1, upper=10> k;
}
parameters {
  real a;
}
model {
  vector[10] literal = rep_vector(0, 10);
  vector[10] symbolic = rep_vector(0, 10);
  vector[10] indirect = rep_vector(0, 10);
  array[2] int idx = {1, 2};
  a ~ std_normal();

  literal[{1, 3}] = rep_vector(a, 2);
  // No warning. The read and write select disjoint sets of elements.
  if (sum(literal[{2, 4}]) > 0) print(literal);

  symbolic[{1, k}] = rep_vector(a, 2);
  // Warn. The write selects element 2 when k is 2.
  if (symbolic[2] > 0) print(symbolic[2]);

  indirect[idx[1]] = a;
  idx[1] = 2;
  indirect[idx[1]] = 0;
  // Warn. Changing idx[1] redirects the second write to element 2.
  if (indirect[1] > 0) print(indirect[1]);
}
