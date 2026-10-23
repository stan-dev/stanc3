/* $ stanc --auto-format ../fun-return-typ4.stan : [exit 0] */
functions {
  real foo() {
    if (1) 
      reject("");
    else {
      if (1) 
        ;
      else 
        reject("");
    }
    reject("");
  }
}

/*
warning: Unreachable statement found, is this intended?
    ┌─ ../fun-return-typ4.stan:2:68
  1 │  functions {
  2 │    real foo() { if (1) reject(""); else {if (1) ; else reject("");} reject("");}
    │                        -----------                                  ^ Never reached
    │                        │
    │                        stops this model evaluation
  3 │  }
*/
