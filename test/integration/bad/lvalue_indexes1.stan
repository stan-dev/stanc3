data {
    array[5] vector[5] x;
}
transformed data {
    array[5] vector[5] y;
    y[:][1] = x[:][1];
}