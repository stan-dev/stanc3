data {
  int N;
}
generated quantities {
  for (var in 1:N) {
    print("foo");
  }
}
