functions {
  @extern real foo(int x, int y);

  @foo @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    print(x);
  }
}
parameters {
  @baz matrix[3,3] A;
}


generated quantities {
  @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
}
