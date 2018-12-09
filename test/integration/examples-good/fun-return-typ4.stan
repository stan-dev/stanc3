functions {
  real foo() { if (1) reject(""); else {if (1) ; else reject("");} reject("");}
}