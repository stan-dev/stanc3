/* $ stanc --auto-format ../jacobian_pe.stan : [exit 0] */
functions {
  real foo() {
    return 1.0;
  }
}
transformed parameters {
  jacobian += foo();
}

