var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let basic_model = `
parameters {
	real y;
}
model {
    z ~ std_normal();
}
`

let basic = stanc.stanc("basic", basic_model, ["filename-in-msg=good_filename"]);
utils.print_error(basic)

let basic_no_args = stanc.stanc("basic", basic_model);
utils.print_error(basic_no_args)
