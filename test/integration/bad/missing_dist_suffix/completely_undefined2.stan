data {
   int foo_lpmf;
}
model {
  target += foo_lpdf(1);
}