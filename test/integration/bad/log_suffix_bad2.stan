functions {
    real foo_log(real alpha, real beta){
     return normal_lupdf(alpha | beta, 1);
   }
}

