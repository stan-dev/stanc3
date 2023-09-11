model {
  real foo = 1.5;
  // should warn
  foo = foo;
  // arguably these are even worse
  array[2] real bar = bar;
  real baz = foo + baz;
  // but this should be fine
  bar[2] = bar[1];
}
