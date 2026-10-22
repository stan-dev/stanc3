/* $ stanc --auto-format ../gauss3D.stan : [exit 0] */
parameters {
  array[3] real x;
}
model {
  x ~ normal(0, 1);
}

