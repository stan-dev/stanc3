data {
  int N;
  vector_cl[N] rating;
}
transformed data {
   vector[N] rating_cpu = from_matrix_cl(rating);
   print(rating_cpu);
}