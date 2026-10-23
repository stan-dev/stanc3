/* $ stanc --auto-format ../comments.stan : [exit 0] */
// this is a line comment */ should stay // /**/

// how ugly # can */ we make /* this one // #

/* one line block */

/* a weird
      ly formated
          block
                */

/**
 * Doc comment
 * @return nothing
 */

data {
  //nothing here either
}
transformed data {
  int N;
  array[N /*test1*/, //test1.5
        N] real /*test2*/ arr;
  
  /*test1*/ //test1.5
  /*test2*/ /*test2.1*/ /*test2.2*/ /*test2.3*/
  tuple(array[N, N] real, real) tup;
  
  int foo, //test3
  bar = 3 //test4
  //test5
  , //test6
  baz; //test7
  
  // 1
  array[3] vector[2] // 2
  // 3
  // 4
  x;
}
model {
  // unicode in comments
  // Љ😃
  /*
  λ β ζ π
  //*/
}

