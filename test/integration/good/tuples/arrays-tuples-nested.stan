data {
  // passing
  (array[10] real, int) basic;
  (int, (real, vector[2])) tuple_tuple;
  array[3] (int, array[4] real, vector[3]) arr_tuple;
  (array[2] real, int, array[3] (real, array[4] int)) tuple_arr_tuple;

  // failing
  // array[2] (real, int, (real, (int, vector[4]))) arr_tuple_tuple;
  // really hard
  // array[3] (int, array[4] (real, vector[5])) arr_tuple_arr_tuple;
}

parameters {
  (array[10] real, real) basic_p;
  (real, (real, vector[2])) tuple_tuple_p;
  array[3] (complex, array[4] real, vector[3]) arr_tuple_p;
  (array[2] real, real, array[3] (real, array[4] real)) tuple_arr_tuple_p;
  array[2] (real, real, (real, (complex, vector[4]))) arr_tuple_tuple_p;
  array[3] (real, array[4] (real, vector[5])) arr_tuple_arr_tuple_p;
}
