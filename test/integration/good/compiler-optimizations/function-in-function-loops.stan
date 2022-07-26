functions {
  real do_something(real x){
    if  (2 == 3)
      return x;
    else
      return x;
  }

  real summer(vector et){
    int N = size(et);
    real running_sum = 0;
    for (n in 1 : N) {
        running_sum += do_something(et[n]);
    }
    return running_sum;
  }

}

data {
  int N;
}

model {
  vector[N] eta = rep_vector(0.0, N);
  real out = summer(eta);
  target += out;
}
