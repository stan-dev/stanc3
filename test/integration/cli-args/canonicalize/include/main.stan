functions {
   #include b.stanfunctions
   #include c.stanfunctions
}

data {
   /* ... declarations ... */
}

#include
  <tdata.stan>

model {
  x ~ bernoulli(0);
  if (1) {

  } else {
#include "tildes.inc"
  }
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
