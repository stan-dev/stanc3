/* $ stanc --auto-format ../prim_var_data_block.stan : [exit 0] */
data {
  int<lower=0, upper=1> p1;
  real p2;
  array[3] int ar_p1;
  array[4] real<lower=0, upper=1> ar_p2;
}

