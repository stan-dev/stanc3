/* $ stanc --auto-format ../good_annotations.stan : [exit 0] */
functions {
  @extern real foo(int x, int y);
  
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
    ┌─ ../good_annotations.stan:4:3
  3 │  
  4 │    @foo @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    │    ^^^^ Unknown annotation 'foo' will be ignored by the compiler.
  5 │      print(x);
warning: Unknown annotation 'biz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:4:8
  3 │  
  4 │    @foo @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    │         ^^^^ Unknown annotation 'biz' will be ignored by the compiler.
  5 │      print(x);
warning: Unknown annotation 'baz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:9:3
  8 │  parameters {
  9 │    @baz matrix[3,3] A;
    │    ^^^^ Unknown annotation 'baz' will be ignored by the compiler.
 10 │  }
warning: Unknown annotation 'bar' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:14:3
 13 │  generated quantities {
 14 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │    ^^^^ Unknown annotation 'bar' will be ignored by the compiler.
 15 │  }
warning: Unknown annotation 'baz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:14:8
 13 │  generated quantities {
 14 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │         ^^^^ Unknown annotation 'baz' will be ignored by the compiler.
 15 │  }
warning: Unknown annotation 'flux' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:14:13
 13 │  generated quantities {
 14 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │              ^^^^^ Unknown annotation 'flux' will be ignored by the compiler.
 15 │  }
warning: Unknown annotation 'really_extra_long_now' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:14:49
 13 │  generated quantities {
 14 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │                                                  ^^^^^^^^^^^^^^^^^^^^^^ Unknown annotation 'really_extra_long_now' will be ignored by the compiler.
 15 │  }
*/
