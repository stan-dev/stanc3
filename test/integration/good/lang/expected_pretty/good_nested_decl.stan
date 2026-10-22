/* $ stanc --auto-format ../good_nested_decl.stan : [exit 0] */
functions {
  real foo() {
    if (1) 
      real this_does_parse = 0, try_me;
    return 0;
  }
}

