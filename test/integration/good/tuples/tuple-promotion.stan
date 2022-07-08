data {
  vector[3] V;
  (array[3] int, array[3] int) d;
}

transformed data {
  (array[2] real, complex) basic = ({1,2},3);

  (complex_vector[3], real) CV = (V, 2);
  (vector[3], int) V2 = (V, 2);
  CV = V2;
}

transformed parameters {
  (array[3] complex, array[3] complex) arrs = d;
}

generated quantities {
  int y = 1;
  (real, real) x = (y, 3);
  (complex, real) z = x;
  (complex, real) z2 = z;
}
