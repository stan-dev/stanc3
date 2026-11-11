/* $ stanc --auto-format ../good_annotations.stan : [exit 0] */
functions {
  @foo(false) @biz void bar(int x,
                            int y,
                            int z,
                            int w,
                            int a,
                            int b,
                            int d,
                            int e,
                            int f) {
    print(x);
  }
}
parameters {
  @baz(1, true, int) matrix[3, 3] A;
}
generated quantities {
  /* comment in an odd place */
  @bar
  @baz
  @flux
  @really_extra_long_now
  matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
}

/*
warning: Unknown annotation '@foo'.
    ┌─ ../good_annotations.stan:2:3
  1 │  functions {
  2 │    @foo(false) @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    │    ^^^^^^^^^^^ Unknown annotation.
  3 │      print(x);
    = This will be ignored by the compiler.
warning: Unknown annotation '@biz'.
    ┌─ ../good_annotations.stan:2:15
  1 │  functions {
  2 │    @foo(false) @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    │                ^^^^ Unknown annotation.
  3 │      print(x);
    = This will be ignored by the compiler.
warning: Unknown annotation '@baz'.
    ┌─ ../good_annotations.stan:7:3
  6 │  parameters {
  7 │    @baz(1, true, int) matrix[3,3] A;
    │    ^^^^^^^^^^^^^^^^^^ Unknown annotation.
  8 │  }
    = This will be ignored by the compiler.
warning: Unknown annotation '@bar'.
    ┌─ ../good_annotations.stan:11:3
 10 │  generated quantities {
 11 │    @bar @baz () @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │    ^^^^ Unknown annotation.
 12 │  }
    = This will be ignored by the compiler.
warning: Unknown annotation '@baz'.
    ┌─ ../good_annotations.stan:11:8
 10 │  generated quantities {
 11 │    @bar @baz () @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │         ^^^^^^^ Unknown annotation.
 12 │  }
    = This will be ignored by the compiler.
warning: Unknown annotation '@flux'.
    ┌─ ../good_annotations.stan:11:16
 10 │  generated quantities {
 11 │    @bar @baz () @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │                 ^^^^^ Unknown annotation.
 12 │  }
    = This will be ignored by the compiler.
warning: Unknown annotation '@really_extra_long_now'.
    ┌─ ../good_annotations.stan:11:52
 10 │  generated quantities {
 11 │    @bar @baz () @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │                                                     ^^^^^^^^^^^^^^^^^^^^^^ Unknown annotation.
 12 │  }
    = This will be ignored by the compiler.
*/
