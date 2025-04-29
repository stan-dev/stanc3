data {
  int N;
  vector[N] d_vec;
}
transformed data {
  real transformed_data_real;
  transformed_data_real = wiener_lpdf(d_vec | d_vec, d_vec, d_vec, d_vec, d_vec);
}
parameters {
  vector[N] p_vec;
}
transformed parameters {
  real transformed_param_real;
  transformed_param_real = wiener_lpdf(p_vec | d_vec, d_vec, p_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, p_vec, d_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, p_vec, p_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, p_vec, p_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, d_vec, p_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, p_vec, d_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, d_vec, p_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, p_vec, d_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, d_vec, d_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, p_vec, d_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, d_vec, d_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, p_vec, d_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, p_vec, d_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, d_vec, d_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, d_vec, p_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, p_vec, p_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, p_vec, d_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, p_vec, p_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, d_vec, d_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, d_vec, d_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, p_vec, d_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, p_vec, p_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, p_vec, d_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, p_vec, p_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, d_vec, p_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, p_vec, d_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, d_vec, d_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, p_vec, p_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, d_vec, p_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, p_vec, d_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, d_vec, p_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, d_vec, d_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, p_vec, p_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, p_vec, p_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, p_vec, p_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, d_vec, d_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, d_vec, p_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, d_vec, d_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, d_vec, p_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, d_vec, p_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, d_vec, d_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, p_vec, p_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, p_vec, d_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, p_vec, p_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, d_vec, d_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, d_vec, p_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, p_vec, p_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, d_vec, d_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, d_vec, d_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, d_vec, d_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, d_vec, p_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | d_vec, d_vec, p_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(p_vec | p_vec, p_vec, p_vec, p_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, p_vec, p_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, p_vec, p_vec, d_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, p_vec, d_vec, d_vec, d_vec);
  transformed_param_real = wiener_lpdf(d_vec | p_vec, d_vec, p_vec, p_vec, p_vec);
  transformed_param_real = wiener_lpdf(d_vec | d_vec, p_vec, d_vec, p_vec, d_vec);
}
model {
  p_vec ~ normal(0, 1);
}
