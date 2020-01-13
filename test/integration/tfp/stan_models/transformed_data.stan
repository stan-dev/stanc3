data { 
    int<lower=0,upper=1> X; 
} 
transformed data {
    int X2;
    X2 = X + 1;
}
parameters { 
    real<lower=0,upper=1> p; 
} 
model { 
    X2 ~ bernoulli(p); 
} 