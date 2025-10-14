Default is filename.hpp
  $ stanc basic.stan
  $ ls
  basic.hpp
  basic.stan
  $ rm basic.hpp

Output file is respected
  $ stanc --o=foo.cpp basic.stan
  $ ls
  basic.stan
  foo.cpp
  $ rm foo.cpp

Output file for formatting prevents cpp generation
  $ stanc --auto-format --o=basic_fmt.stan basic.stan
  $ ls
  basic.stan
  basic_fmt.stan
  $ cat basic_fmt.stan
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
  $ rm basic_fmt.stan

Output file isn't present in C++ args array
  $ stanc --O -fno-soa --o=foo.cpp basic.stan
  $ grep "stancflags" foo.cpp
               "stancflags = --O -fno-soa"};
  $ rm foo.cpp

Error on un-writable output file
  $ touch basic.hpp
  $ chmod -w basic.hpp
  $ stanc --o=basic.hpp basic.stan
  Error writing to file 'basic.hpp': basic.hpp: Permission denied
  [1]
  $ rm -f basic.hpp
