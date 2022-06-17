data { 
}
model {  
    target += binomial_logit_cdf(1|0,1);
}
