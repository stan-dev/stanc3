parameters {
  real y;
}
transformed parameters {
  real no_init;
  real init_from_param = y * 5.0;
  real dependent_no_init = no_init * y;
  real used_on_lhs = used_on_lhs;
  real no_init_if;
  if (1 == 1) {
    no_init_if = 1.0;
  }
  real for_loop_var;
  for (i in 1:10) {
    for_loop_var = 1;
  }
}