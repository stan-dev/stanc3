functions {
  array[] complex foo(real a){
    return {1, 2};
  }

  complex_vector bar(real a){
    return [1, 2+a]';
  }
}

parameters {
  real a;
}

transformed parameters {
  array[2] complex b = foo(a);
  complex_vector[2] c = bar(a);
}
