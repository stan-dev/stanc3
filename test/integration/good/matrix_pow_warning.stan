data {
   int<lower=0> N;
   matrix[N,N] A;
   int power;
}

transformed data {
   matrix[N,N] A_pow = A ^ power;
   matrix[N,N] A_pow_r = A ^ 3.5;

}
