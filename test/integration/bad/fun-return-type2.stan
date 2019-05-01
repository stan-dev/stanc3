functions {
  real foo() { if (1) reject(""); else {if (1) return {5}; reject("");}}
}