data { 
}
model {  
    // known family, known suffix, not implemented  
    target += normal_lpmf(1|0,1);
}
