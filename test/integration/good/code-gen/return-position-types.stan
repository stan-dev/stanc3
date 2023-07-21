functions {
  array[] real foo(real a) {
    return {0.1};
  }

    tuple(array[] real, real) baz(real a) {
    return ({0.1},0.2);
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
  array[1] real z3 = baz(a).1;

}
