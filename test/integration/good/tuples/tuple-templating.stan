functions {
  void foo(tuple(matrix, int) test) {
    print(test.1);
  }
  
  real tsum(tuple(array[] int, array[] real) s) {
    return sum(s.2);
  }
  
  void foo2(array[] tuple(matrix, int) test) {
    print(test[1].1);
  }
  
  void foo3(tuple(real, matrix) test) {
    print(test.1);
  }
  
  void overly_complicated(tuple(array[] matrix, tuple(int, matrix), real) t1,
                          array[] tuple(int, matrix) t2) {
    print(t1.2.2);
  }
}
data {
  int N;
  matrix[N, N] m1;
  matrix[N, N] m2;
  array[N] int a1;
  array[N] real a2;
}
generated quantities {
  // eigen expression inside tuple
  foo((m1 + m2, 1));
  // different types inside tuple
  real s = tsum((a1, a2));
  // eigen expression inside tuple inside array
  foo2({(m1 + m2, 1)});
  
  // a whole bunch of painful
  overly_complicated(({m1 + m2}, (1, m1 + m2), 3.5),
                     {(1, m1 * 2), (2, m2 * 3)});
  overly_complicated(({m1}, (1, m1), 3.5), {(1, m1), (2, m2)});
}
