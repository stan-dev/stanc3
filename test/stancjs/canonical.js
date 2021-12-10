var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let basic_model = `
transformed data {
    real a = multiply_log(4,5);
}
parameters {
	real y;
}
model {
    y ~ normal(((((((((0)))))))),1);
}
`

let basic = stanc.stanc("basic", basic_model, ["print-canonical"]);
utils.print_error(basic)
utils.print_result(basic)

let deps_only = stanc.stanc("basic", basic_model, ["auto-format", "canonicalize=deprecations"]);
utils.print_error(deps_only)
utils.print_result(deps_only)

let everything_but = stanc.stanc("basic", basic_model, ["auto-format", "canonicalize=braces,parentheses"]);
utils.print_error(everything_but)
utils.print_result(everything_but)
