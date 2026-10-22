/* $ stanc --auto-format ../fun-return-typ2.stan : [exit 0] */
functions {
  real foo() {
    if (1) 
      reject("");
    reject("");
  }
}

/*
warning: Unreachable statement found, is this intended?
    ┌─ ../fun-return-typ2.stan:2:34
  1 │  functions {
  2 │    real foo() { if(1) reject(""); reject("");}
    │                       ----------- ^ Never reached
    │                       │
    │                       stops this model evaluation
  3 │  }
*/
