functions {
  real foo(data tuple(real, real) x) {
    return x.1;
  }
  real bar(data array[] tuple(real, real) x) {
    return x[1].2;
  }
  real baz(data array[] tuple(array[] matrix, real) x) {
    return x[1].1[1][1, 1];
  }
}
data {
  tuple(real, real) d;
}
transformed data {
  array[2] tuple(real,real) d2 = {d, d};

  matrix[1, 1] m = [[1]];
  array[1] matrix[1, 1] m2 = {m};
  tuple(array[1] matrix[1, 1], real) x = (m2, d.2);
  array[1] tuple(array[1] matrix[1, 1], real) x2 = {x};
}
model {
  target += foo(d);
  target += bar(d2);
  target += bar({d, d});
  target += baz(x2);
  target += baz({x});
  target += baz({({m}, d.2)});
  target += baz({(m2, d.2)});
  target += baz({({[[1]]}, d.2)});
}
