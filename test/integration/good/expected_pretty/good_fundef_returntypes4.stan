/* $ stanc --auto-format ../good_fundef_returntypes4.stan : [exit 0] */
functions {
  real foo() {
    if (215) 
      return 24;
    else 
      reject("");
  }
}

