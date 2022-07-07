data {
  vector[3] V;
}

transformed data {
  (array[2] real, complex) basic = ({1,2},3);

  (complex_vector[3], real) CV = (V, 2);
  (vector[3], int) V2 = (V, 2);
  CV = V2;
}

generated quantities {
    int y = 1;
   (real, real) x = (y, 3);
   (complex, real) z = x;
   (complex, real) z2 = z;
}
