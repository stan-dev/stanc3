data { 
}
model {  
    target += binomial_logit_lccdf(1|0,1);
}
