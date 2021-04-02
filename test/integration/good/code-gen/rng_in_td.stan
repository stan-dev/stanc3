functions {
  real do_rng() {
    return normal_rng(0, normal_rng(1, 10));
  }
}
transformed data {
  array[3] real xx = {1, 2, discrete_range_rng(1, 10)};
  array[2, 2] real xxx = {{1, discrete_range_rng(1, 10)},
                          {3, discrete_range_rng(1, 10)}};
  {
    while (discrete_range_rng(0, 10)) {
      if (discrete_range_rng(1, 10)) {
        for (i in (discrete_range_rng(1, 10)) : (add(binomial_rng(10, .3),
                                                     discrete_range_rng(
                                                     1, 10)))) {
          {
            xx[1] = normal_rng(0, 1);
            xx[discrete_range_rng(1, 10)] = add(do_rng(), do_rng());
            xxx[1, discrete_range_rng(1, 10)] = normal_rng(0, 1);
          }
        }
      }
      else {
        for (i in (discrete_range_rng(1, 10)) : (add(binomial_rng(10, .3),
                                                     discrete_range_rng(
                                                     1, 10)))) {
          {
            xx[1] = normal_rng(0, 1);
            xx[discrete_range_rng(1, 10)] = add(do_rng(), do_rng());
            xxx[1, discrete_range_rng(1, 10)] = normal_rng(0, 1);
          }
        }
      }
      break;
    }
  }
}

