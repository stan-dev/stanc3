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
model {
  target += foo(d);
  target += bar({d, d});
  target += baz({({[[1]]}, d.2)});
}
