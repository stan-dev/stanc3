/* $ stanc --auto-format --include-paths=.. --canonicalize=includes ../main.stan : [exit 0] */
functions {
  // line comment a
  /*weird block a*/
  void foo(real x) {
    return;
  }
  // another line comment a
  
  //include other functions
  // line comment b
  /*weird block b*/
  void bar(real y) {
    return;
  }
  // another line comment b
  
  /**
    A documentation comment
  */
  int baz(real z) {
    if (1) 
      return 2; // test
    else 
      return 3;
    // trailing coment in c
  }
}
data {
  /* ... declarations ... */
}
// I'm in another file!

transformed data {
  int x = 30;
}
model {
  x ~ bernoulli(0);
  if (1) {
    
  } else {
    // test tildes
    
    x ~ exponential(15);
  }
}
generated quantities {
  real z;
  if (1) {
    
  } else {
    // test 2
    
    z = exp(15);
    
    /* this contains only statements
    */
    
    z = 10.5; // z = 10.5
    z = 11.2;
  }
  
  if (1) {
    /* block comment */
    
    foo(3.4);
  }
}

