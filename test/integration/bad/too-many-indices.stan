transformed data {
  array[2] matrix[2,3] a = { [ [1,2,3], [4,5,6] ],
		       [ [2,2,3], [2,5,6] ] };
  vector[2] b = a[1, 1, 1:2, 1];
}
