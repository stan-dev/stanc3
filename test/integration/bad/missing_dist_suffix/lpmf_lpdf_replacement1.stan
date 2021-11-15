data { 
}
model {  
    // known family, known suffix, not implemented  
    target += binomial_lpdf(1|0,1);
}
