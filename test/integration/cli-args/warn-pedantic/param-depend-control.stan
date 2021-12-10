parameters {
  real a;
  real b;
}
model {
  int x;
  int y = 0;
  if(a > 0) {
    x = 0;
  } else {
    x = 1;
  }
  for(i in 0:x) {
    y = y + 1;
  }
  while ( y > 0 ) {
    b ~ normal(0, 1);
  }
}
