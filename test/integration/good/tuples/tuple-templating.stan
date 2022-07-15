functions {
  void foo((matrix,) test){
    print(test.1);
  }

  real tsum((array[] int, array[] real) s){
    return sum(s.2);
  }

  void foo2(array[] (matrix,) test){
    print(test[1].1);
  }

  void foo3((real, matrix) test){
    print(test.1);
  }

  void overly_complicated((array[] matrix, (int, matrix)) t1, array[](int, matrix)t2){
    print(t1.2.2);
  }
}

data {
  int N;
  matrix[N,N] m1;
  matrix[N,N] m2;
  array[N] int a1;
  array[N] real a2;
}

generated quantities {
  // eigen expression inside tuple
  foo((m1+m2,));
  // different types inside tuple
  real s = tsum((a1,a2));
  // eigen expression inside tuple inside array
  foo2({(m1+m2,)});

  // a whole bunch of painful
  overly_complicated(({m1+m2}, (1, m1)), {(1,m1), (2, m2)});
}
