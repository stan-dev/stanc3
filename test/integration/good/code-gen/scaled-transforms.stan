data {
   int<lower=0> N;
}

parameters {
    array[3] real<lower=0><offset=10> loweroffset;
    real<lower=0, upper=1><offset=1, multiplier=10> allfour;
    simplex<offset=1>[10] offset_simplex;
    positive_ordered<multiplier=10>[N] multiplied_ordered; 
}