data {
  int N;
}
generated quantities {
  int foo;

  for (foo in 1:N) {
    print("foo");
  }
}
