data {
  tuple(int<lower=0>, tuple(real<lower=1>, int<upper=2>)) y;
  tuple(int<lower=0>, real<lower=1>, simplex[2]) x;
  array[3,2] tuple(real<lower=0>, simplex[4]) z;
}
