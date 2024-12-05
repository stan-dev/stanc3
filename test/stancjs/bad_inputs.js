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

let bad_code_type = stanc.stanc("basic", basic_model.split('\n'), []);
utils.print_error(bad_code_type)

let bad_name_type = stanc.stanc(1234.5, basic_model, []);
utils.print_error(bad_name_type)

let bad_flags_type = stanc.stanc("basic", basic_model, 1234.5);
utils.print_error(bad_flags_type)

let bad_flag_type = stanc.stanc("basic", basic_model, ["foo", 1234.5]);
utils.print_error(bad_flag_type)
