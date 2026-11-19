parameters {
  real a;
}
model {
  vector[10] ranges = rep_vector(0, 10);
  vector[10] nested = rep_vector(0, 10);
  vector[10] cleared = rep_vector(0, 10);
  a ~ std_normal();

  ranges[1:2] = rep_vector(a, 2);
  // No warning. The write ends at element 2.
  if (sum(ranges[3:4]) > 0) print(ranges);
  // Warn. Both slices include element 2.
  if (sum(ranges[2:3]) > 0) print(ranges);

  nested[2] = a;
  // Warn. The first element of the slice is element 2 of nested.
  if (nested[2:3][1] > 0) print(nested);

  cleared[1] = a;
  cleared[1:2] = rep_vector(0, 2);
  // Warn. The analysis retains the earlier element write across a slice write.
  if (cleared[1] > 0) print(cleared);
}
