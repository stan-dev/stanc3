data { 
}
model {  
    target += binomial_logit_lcdf(1|0,1);
}
