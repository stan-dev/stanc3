/* $ stanc --auto-format ../num_args.stan : [exit 0] */
functions {
  real foo(real one) {
    return 1.0;
  }
  
  real foo(real one, real two) {
    return 2.0;
  }
}

