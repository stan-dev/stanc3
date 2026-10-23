/* $ stanc --auto-format ../good_fundef_returntypes1.stan : [exit 0] */
functions {
  real foo() {
    if (215) 
      return 24;
    return 24;
  }
}

/*
warning: Unreachable statement found, is this intended?
    ┌─ ../good_fundef_returntypes1.stan:5:5
  3 │      if (215)
  4 │        return 24;
    │        ---------- exits the function
  5 │      return 24;
    │      ^ Never reached
  6 │    }
*/
