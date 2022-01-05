functions{
  real twoCptModelODE(real t,
                      array[] real x,
                      array[] real parms,
                      array[] real rdata,
                      array[] int idata){
    array[2] real dxdt;
    return dxdt[2];
  }
}
model {
  array[2, 2] real x
    = integrate_ode_bdf(twoCptModelODE,
                        {1, 1.3}, 1.0, { 2.2, 3 }, { 1.0 }, { 1.0 }, { 2 },
                        10, 10, 10);
}
