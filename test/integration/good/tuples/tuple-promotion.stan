functions {
   real dummy((array[] real, array[] real) test){
    return sum(test.1) + sum(test.2);
   }
}

data {
  vector[3] V;
  (array[3] int, array[3] int) d;
}

transformed data {
  array[4] (int, array[2] int) arrs;

  ((int, array[2] int), int, (vector[3], int)) nested;

  (array[2] real, complex) basic = ({1,2},3);

  (complex_vector[3], real) CV = (V, 2);
  (vector[3], int) V2 = (V, 2);
  CV = V2;

  real t = dummy(d);
}

transformed parameters {
  (array[3] complex, array[3] complex) d2 = d;
  (vector[3], real) V3 = V2;

  array[4] (real, array[2] complex) arrs2 = arrs;

  ((real, array[2] complex), real, (vector[3], real)) nested2 = nested;
  ((real, array[2] complex), real, (vector[3], real)) nested3;
  nested3 = nested;
}

generated quantities {
  int y = 1;
  (real, real) x = (y, 3);
  (complex, real) z = x;
  (complex, real) z2 = z;
}
