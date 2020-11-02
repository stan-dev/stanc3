functions {
  real f(array[,,] real array) { return array[1,2,3]; }
}

model {
  real array;
  array[1,2,3] real abc;
}
