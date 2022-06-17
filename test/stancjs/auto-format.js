var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let basic_model = `
parameters {
real y;
}
transformed parameters {
print("this line is much, much, much ", "too long", " to print on one line");
}
model {
y~std_normal();
}
`

let basic = stanc.stanc("basic", basic_model, ["auto-format"]);
utils.print_result(basic)
let short = stanc.stanc("basic", basic_model, ["auto-format", "max-line-length=40"]);
utils.print_result(short)
