/**
 * Test whether the monotone framework goes deep enough to see X_tp1 should not 
 *  be data
 */

data {
    matrix[10, 10] X_d;
}
parameters {
    matrix[10, 10] X_p;
}

transformed parameters {
    matrix[10, 10] X_tp1 = X_d;
    matrix[10, 10] X_tp2 = exp(X_tp1);
    matrix[10, 10] X_tp3 = exp(X_tp2);
    matrix[10, 10] X_tp4 = exp(X_tp3);
    matrix[10, 10] X_tp5 = exp(X_tp4);
    matrix[10, 10] X_tp6 = exp(X_tp5);
    matrix[10, 10] X_tp7 = exp(X_tp6);
    X_tp1 = X_p;
} 