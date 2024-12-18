Default is filename.hpp
  $ stanc basic.stan && ls && rm basic.hpp
  basic.hpp
  basic.stan

Output file is respected
  $ stanc --o=foo.cpp basic.stan && ls && rm foo.cpp
  basic.stan
  foo.cpp

Output file for formatting prevents cpp generation
  $ stanc --auto-format --o=basic_fmt.stan basic.stan && ls && cat basic_fmt.stan && rm basic_fmt.stan
  basic.stan
  basic_fmt.stan
  data {
    int<lower=0> N;
    array[N] int<lower=0, upper=1> y;
  }
  parameters {
    real<lower=0, upper=1> theta;
  }
  model {
    theta ~ beta(1, 1); // uniform prior on interval 0,1
    y ~ bernoulli(theta);
  }

Output file isn't present in C++ args array
  $ stanc --O -fno-soa --o=foo.cpp basic.stan && grep "stancflags" foo.cpp && rm foo.cpp
               "stancflags = --O1 -fno-soa"};

Error on un-writable output file
  $ touch basic.hpp && chmod -w basic.hpp && stanc --o=basic.hpp basic.stan
  Error writing to file 'basic.hpp': basic.hpp: Permission denied
  [1]
