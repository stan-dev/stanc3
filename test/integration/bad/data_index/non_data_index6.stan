parameters {
  array[3] real y;

}
transformed parameters {
  simplex[size(y)] z;
}
