data {
  array[10] int N;
}
generated quantities {
  int foo;

  for (foo in N) {
    print("foo");
  }
}
