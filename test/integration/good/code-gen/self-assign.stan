generated quantities {
  int i = 3;

  i = i;

  tuple(real, int) x = (1.5, i);

  x.1 = x.1;

  array[20] real r;
  for (j in 1 : 20)
    r[to_int(uniform_rng(1, 20))] = r[to_int(uniform_rng(1, 20))];
}
