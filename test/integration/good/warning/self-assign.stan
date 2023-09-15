model {
  real foo = 1.5;
  // line 4: should warn
  foo = foo;
  // line 6: should warn (decl)
  array[2] real bar = bar;
  // line 8: should warn (decl)
  real baz = foo + baz;
  // no warnings
  bar[2] = bar[1];
  //line 12: should warn
  bar[1] = bar[1];

  // line 15: should warn
  foo = (foo);
  // no warnings
  foo = -foo;
  foo = (foo > 0) ? foo : 0.0;

  tuple(real, real) tup = (1.0, 2.0);
  // line 22: should warn
  tup.1 = tup.1;
  // line 24: should warn (decl)
  tuple(real, real) tup2 = (0.5, tup2.1);
  // line 26: could warn, currently does not
  tup = (tup.1, tup.2);
  // line 28: could warn, currently does not
  foo = (foo, foo).1;
  // no warnings
  real foo2 = 3;
  foo = (foo, foo2).2;
}
