functions {
  vector foo(vector oR) {
    // if the inliner incorrect substitutes names, this will fail
    //   due to use-before-def
    int ot = num_elements(oR);

    vector[ot] R = oR;

    return (R);
  }
}
data {
  int N;
}
transformed parameters {
  vector[N] R;
  vector[N] out;
  out = foo(R);
}
