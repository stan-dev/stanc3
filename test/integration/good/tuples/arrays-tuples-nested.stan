data {
  array[10] (real, real) darr;
  array[10] (real, array[10] (real, real)) darr2;
  (real, array[10] (real, real)) darr3;
  array[11] (array[10] real, array[14] (real, int)) darr4;

}

parameters {
  array[10] (real, real) arr;
  array[10] (real, array[10] (real, real)) arr2;
  (real, array[10] (real, real)) arr3;
  array[11] (array[10] real, array[14] (real, simplex[2])) arr4;
}
