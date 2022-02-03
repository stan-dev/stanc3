functions {
  real foo(int p){
    return p + 1.0;
  }

   real foo(real p) {
   	return p + 2;
   }
   real foo(row_vector p) {
   	return sum(p) + 3;
   }
   real foo(vector p) {
   	return sum(p) + 4;
   }
   real foo(matrix p) {
   	return sum(p) + 5;
   }
   real foo(array[] real p) {
   	return sum(p) + 6;
   }

  real foo(array[] row_vector p) {
   	return sum(p[1]) + 7;
   }
   real foo(array[] vector p) {
   	return sum(p[1]) + 8;
   }
   real foo(array[] matrix p) {
   	return sum(p[1]) + 9;
   }
   real foo(array[,] real p) {
   	return sum(p[1]) + 10;
   }
  real foo(array[] int p){
    return sum(p) + 12;
  }
}
transformed data{
   vector[5] a = [0.1, 0.1, 0.1, 0.1, 0.1]';
}
parameters {
    real y;
    real z;
}
model {
    y ~ normal(foo(1), 1);
    y ~ normal(foo(1.5), 1);
    a ~ normal(foo(z), 1);
    y ~ normal(foo(a), 1);
    y ~ normal(foo(a'), 1);
    y ~ normal(foo({a}), 1);
    y ~ normal(foo({1,2}), 1);
    y ~ normal(foo({1,2.3}), 1);
    {5.5,6.5} ~ normal(foo({z,2.3}), 1);
}
