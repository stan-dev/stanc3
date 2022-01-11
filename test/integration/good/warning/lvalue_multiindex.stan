data {
   array [5] matrix[5,5] x;
}
transformed data {
   matrix[5,5] y;
   y[:][:] = x[1][:][:];
   y[:][1] = x[1][:][2];
   array[5] matrix[5,5] z;
   z[:][:,1] = x[:][:,1];
}