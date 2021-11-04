generated quantities {
  // make sure pp_recursive_ifthenelse properly places else
  // should be formatted like `} else {` unless a comment is in way
  if (1) {
    // something
  }
  else {
    // something else
  }

  if (1) {
    // something
  } else {
    // something else
  }

  if (1) {
    // something
  } else { /* something else */  }

  if (1) {
    // something
  }
  else  if (0) { /* something else */  } // test
  else {}

  int x;
  if (0) x = 1; /* test */ else x=2;

  if (1) {
    // something
  }


  else {
    // something else
  }
}
