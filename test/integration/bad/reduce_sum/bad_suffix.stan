functions {
  real partial_sum_lp(int[] slice_n_redcards,
                        int start, int end,
                        int[] n_games,
                        vector rating,
                        vector beta) {
    return binomial_logit_lpmf(slice_n_redcards |
                                  n_games[start:end],
                                beta[1] + beta[2] * rating[start:end]);
  }
}
data {
  int<lower=0> N;
  int<lower=0> n_redcards[N];
  int<lower=0> n_games[N];
  vector[N] rating;
  int<lower=1> grainsize;
}
parameters {
  vector[2] beta;
}
model {

  beta[1] ~ normal(0, 10);
  beta[2] ~ normal(0, 1);

  target += reduce_sum(partial_sum_lp, n_redcards, grainsize,
                       n_games, rating, beta);
}
