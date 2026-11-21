functions {
  @silent real foo(real theta){ return 2 * theta; }
}

transformed data {
  @silent int bar;
}

parameters {
  @not_a_real_annotation real sigma;


  @extern real theta;
}
