functions {
  array[] real foo(real a) {
    return {0.1};
  }
  
  array[] complex bar(real a) {
    return {0.1 + 0.3i};
  }
}
parameters {
  real a;
}
model {
  array[1] real z = foo(a);
  array[1] complex z2 = bar(a);
}
