/* $ stanc --auto-format ../good_fundef_returntypes3.stan : [exit 0] */
functions {
  real foo() {
    if (215) 
      return 24;
    reject("");
  }
}

/*
warning: Unreachable statement found, is this intended?
    ┌─ ../good_fundef_returntypes3.stan:5:5
  3 │      if (215)
  4 │        return 24;
    │        ---------- exits the function
  5 │      reject("");
    │      ^ Never reached
  6 │    }
*/
