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

let bad_flags_type1 = stanc.stanc("basic", basic_model, 1234.5);
utils.print_error(bad_flags_type1)

let bad_flags_type2 = stanc.stanc("basic", basic_model, {});
utils.print_error(bad_flags_type2)

let bad_flags_type3 = stanc.stanc("basic", basic_model, "Flags flags flags");
utils.print_error(bad_flags_type3)

let bad_flag_type = stanc.stanc("basic", basic_model, ["foo", 1234.5]);
utils.print_error(bad_flag_type)
