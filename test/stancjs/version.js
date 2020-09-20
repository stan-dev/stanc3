var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let version = stanc.stanc("version-test", "", ["version"]);
utils.print_result(version)

let basic_model = `
parameters {
	real y;
}
model {
    y ~ std_normal();
}
`


let version_model = stanc.stanc("version-test", basic_model, ["version"]);
utils.print_result(version_model)

let version_model_multiple_flags = stanc.stanc("version-test", basic_model, ["warn-pedantic", "version"]);
utils.print_result(version_model_multiple_flags)
