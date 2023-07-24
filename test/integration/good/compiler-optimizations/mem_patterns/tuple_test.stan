
parameters {
    tuple(array[1] matrix[1,1], matrix[1,2], matrix[1,3]) xx;
}
transformed parameters {
    matrix[1,1] x = xx.1[1];
}