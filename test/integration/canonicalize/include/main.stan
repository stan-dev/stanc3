data {
   /* ... declarations ... */
}

#include <tdata.stan>

model {
  // int x = 3;
   1 ~ bernoulli(0);
}

generated quantities {
  #include "statements.inc"
}
