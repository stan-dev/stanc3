transformed data {
  real x[3];
}
parameters {
  real xx[3];
}
transformed parameters {
  real y[3,3] = {x,xx,xx};
  real w[3,3] = {{1.0,2,3},xx,xx};
  real td_arr33[3,3]
      = { {1 , 2 , 3 }, 
          {1 , 2., 3 }, 
          {1., 2., 3 } };
}