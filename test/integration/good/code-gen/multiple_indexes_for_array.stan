data{
    real y;
}

transformed data {
    int arr[2,2];
    arr[1,1:2] = {1, 1};

    real x[2] = {1,2};
}

parameters{
    real mu;
}

model{
    y ~ normal(mu, 1);
}