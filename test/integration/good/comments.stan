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
      array[N/*test1*/ , //test1.5
    N/*test2*/] real arr;

    int foo, //test3
    bar, //test4
    baz; //test5
}

model {
  // unicode in comments
  // Љ😃
  /*
  λ β ζ π
  //*/
}
