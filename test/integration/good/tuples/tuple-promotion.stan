functions {
  real dummy(tuple(array[] real, array[] real) test) {
    return sum(test.1) + sum(test.2);
  }
}
data {
  vector[3] V;
  tuple(array[3] int, array[3] int) d;
}
transformed data {
  array[4] tuple(int, array[2] int) arrs;
  
  tuple(tuple(int, array[2] int), int, tuple(vector[3], int)) nested;
  
  tuple(array[2] real, complex) basic = ({1, 2}, 3);
  
  tuple(complex_vector[3], real) CV = (V, 2);
  tuple(vector[3], int) V2 = (V, 2);
  CV = V2;
  
  real t = dummy(d);
}
transformed parameters {
  tuple(array[3] complex, array[3] complex) d2 = d;
  tuple(vector[3], real) V3 = V2;
  
  array[4] tuple(real, array[2] complex) arrs2 = arrs;
  
  tuple(tuple(real, array[2] complex), real, tuple(vector[3], real)) nested2 = nested;
  tuple(tuple(real, array[2] complex), real, tuple(vector[3], real)) nested3;
  nested3 = nested;
}
generated quantities {
  int y = 1;
  tuple(real, real) x = (y, 3);
  tuple(complex, real) z = x;
  tuple(complex, real) z2 = z;
}
