functions {
  void f(int n) {
    for (i in 1:n) {
      if (i == 2) {
        return;
      }
      print(i);
    }
    print("after");
  }
}
data {
  int N;
}
model {
  f(N);
}
