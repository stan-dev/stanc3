data {
    matrix[10, 10] X_data;
}

parameters {
    matrix[10, 10] X_p;
}

transformed parameters {
    matrix[10, 10] X_tp1 = exp(X_data);
    matrix[10, 10] X_tp2 = exp(X_tp1);
    X_tp1 = X_p;
}
