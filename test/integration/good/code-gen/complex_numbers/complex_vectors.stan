

transformed parameters {
   complex_vector[3] z = [3, 4i, 4]';
   complex_matrix[2,2] zs = [[1,2],[3,4]];
   row_vector[2] x = [1,2];
   complex_row_vector[2] zx = x;
}
