data {
  array[2] int T;
}
model {
  1 ~ normal(0.2, 0.1) T[T, ];
}
