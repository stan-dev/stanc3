data {
  tuple(array[10] real, int) basic;
  tuple(int, tuple(real, vector[2])) tuple_tuple;
  array[3] tuple(int, array[4] real, vector[3]) arr_tuple;
  tuple(array[2] real, int, array[3] tuple(real, array[4] int)) tuple_arr_tuple;
  array[2] tuple(real, int, tuple(real, tuple(int, vector[4]))) arr_tuple_tuple;
  array[3] tuple(int, array[4] tuple(real, vector[5])) arr_tuple_arr_tuple;
  array[3] tuple(int, array[4]
                 tuple(real, array[5] tuple(complex, matrix[6, 7]))) very_deep;
}
parameters {
  tuple(array[10] real, real) basic_p;
  tuple(real, tuple(real, vector[2])) tuple_tuple_p;
  array[3] tuple(complex, array[4] real, vector[3]) arr_tuple_p;
  tuple(array[2] real, real, array[3] tuple(real, array[4] real)) tuple_arr_tuple_p;
  array[2] tuple(real, real, tuple(real, tuple(complex, vector[4]))) arr_tuple_tuple_p;
  array[3] tuple(real, array[4] tuple(real, vector[5])) arr_tuple_arr_tuple_p;
  array[3] tuple(real, array[4]
                 tuple(real, array[5] tuple(complex, matrix[6, 7]))) very_deep_p;
}
