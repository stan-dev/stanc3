functions {
  matrix foo(int N, int M){
    return rep_matrix(1, N, M);
  }
}

transformed data {
   int N = 10;
   int M = 11;
   matrix[N,M] bar = foo(N, M);
}

parameters {
  real alpha;
}

model {
  sum(bar) ~ normal(alpha, 0.1);
}
