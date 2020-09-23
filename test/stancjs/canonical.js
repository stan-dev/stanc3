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

