var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let bad = `
parameters {
	real y[10];
}
model {
    target += std_normal_log(y);
}
`

let basic_bad = stanc.stanc("basic_bad", bad, []);
utils.print_error(basic_bad)

let updated = stanc.stanc("basic_bad", bad, ["auto-format", "canonicalize=deprecations"])
utils.print_error(updated)
utils.print_result(updated)
