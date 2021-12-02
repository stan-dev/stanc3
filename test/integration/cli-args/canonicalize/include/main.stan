functions {
   #include test.stanfunctions
}

data {
   /* ... declarations ... */
}

#include <tdata.stan>

model {
  // int x = 3;
   1 ~ bernoulli(0);
}

generated quantities {
  real z;
  if (1) {

  } else {
#include "statements1.inc"
  }

  if (1) {
    #include "statements2.inc"

  }
}
