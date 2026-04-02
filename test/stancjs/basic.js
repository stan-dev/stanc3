var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let basic_model = `
parameters {
	real y;
}
model {
    y ~ std_normal();
}
`

let basic = stanc.stanc("basic", basic_model, []);
utils.print_error(basic)

let basic_no_args = stanc.stanc("basic", basic_model);
utils.print_error(basic_no_args)

let basic_model_err = `
parameters {
	real y;
}
model {
    z ~ std_normal();
}
`

let basic_err = stanc.stanc("basic_err", basic_model_err, []);
utils.print_error(basic_err)

let basic_err_color = stanc.stanc("basic_err", basic_model_err, ["color-output"]);
console.log("With ANSI color output:");
utils.print_error(basic_err_color)
