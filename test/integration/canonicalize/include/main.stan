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
#include "statements.inc"
  }
}
