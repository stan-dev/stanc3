functions {
   real foo_t_log(real alpha, real beta){
     return 1.0;
   }
}

model {
  1.5 ~ foo_t(3.4);
}
