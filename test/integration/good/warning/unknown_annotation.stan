functions {
  @silent real foo(real theta){ return 2 * theta; }
}

parameters {
  @not_a_real_annotation real sigma;


  @extern real theta;
}
