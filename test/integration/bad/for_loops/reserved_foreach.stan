data {
  array[10] int N;
}
generated quantities {
  for (var in N) {
    print("foo");
  }
}
