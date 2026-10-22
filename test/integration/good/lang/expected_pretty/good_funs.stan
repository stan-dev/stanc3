/* $ stanc --auto-format ../good_funs.stan : [exit 0] */
transformed data {
  real x;
  real y;
  real z;
  int n;
  
  z = ((n) ? x : y);
  
  z = lchoose(x, y);
}
model {
  
}

