functions {
   real test_log_lpmf(int bar, real foo){
    return foo;
   }
  real test_log_lpdf(real bar, real foo){
    return foo;
   }

}

model {
  1 ~ test_log_lpmf(2.5);
  1.2 ~ test_log_lpdf(2.5);
}
