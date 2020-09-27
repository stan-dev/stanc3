var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let basic_model = `
parameters {
real y;
}
model {
y~std_normal();
}
`

let basic = stanc.stanc("basic", basic_model, ["auto-format"]);
utils.print_result(basic)
