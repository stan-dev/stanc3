/* $ stanc --auto-format ../good_annotations.stan : [exit 0] */
functions {
  @foo @biz void bar(int x,
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
  @baz matrix[3, 3] A;
  @extern real theta;
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
warning: Unknown annotation 'foo' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:2:3
  1 │  functions {
  2 │    @foo @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    │    ^^^^ Unknown annotation 'foo' will be ignored by the compiler.
  3 │      print(x);
warning: Unknown annotation 'biz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:2:8
  1 │  functions {
  2 │    @foo @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    │         ^^^^ Unknown annotation 'biz' will be ignored by the compiler.
  3 │      print(x);
warning: Unknown annotation 'baz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:7:3
  6 │  parameters {
  7 │    @baz matrix[3,3] A;
    │    ^^^^ Unknown annotation 'baz' will be ignored by the compiler.
  8 │    @extern real theta;
warning: Unsupported annotation 'extern' for type real. This will be ignored by the compiler.
    ┌─ ../good_annotations.stan:8:3
  7 │    @baz matrix[3,3] A;
  8 │    @extern real theta;
    │    ^^^^^^^ Unsupported annotation 'extern' for type real. This will be ignored by the compiler.
  9 │  }
warning: Unknown annotation 'bar' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:13:3
 12 │  generated quantities {
 13 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │    ^^^^ Unknown annotation 'bar' will be ignored by the compiler.
 14 │  }
warning: Unknown annotation 'baz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:13:8
 12 │  generated quantities {
 13 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │         ^^^^ Unknown annotation 'baz' will be ignored by the compiler.
 14 │  }
warning: Unknown annotation 'flux' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:13:13
 12 │  generated quantities {
 13 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │              ^^^^^ Unknown annotation 'flux' will be ignored by the compiler.
 14 │  }
warning: Unknown annotation 'really_extra_long_now' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:13:49
 12 │  generated quantities {
 13 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │                                                  ^^^^^^^^^^^^^^^^^^^^^^ Unknown annotation 'really_extra_long_now' will be ignored by the compiler.
 14 │  }
*/
