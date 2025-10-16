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


          tuple(array[N/*test1*/ , //test1.5
    N/*test2*/]real/*test2.1*/, /*test2.2*/real /*test2.3*/)  tup;


    int foo, //test3
    bar //test4
    = 3 //test5
    , //test6
    baz; //test7
}

model {
  // unicode in comments
  // Љ😃
  /*
  λ β ζ π
  //*/
}
