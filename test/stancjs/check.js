var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let basic_model = `
parameters {
	real y;
}
model {
    real x;
    y ~ normal(0, x);
    x = 1;
}
`

let basic = stanc.check_model("basic", basic_model, ["warn-uninitialized"]);
utils.print_json(basic);

let basic_model_err = `
parameters {
	real y;
}
model {
    z ~ std_normal();
}
`

let basic_err =  stanc.check_model("basic_err", basic_model_err, []);
utils.print_json(basic_err);

console.log("Error expected:");
try {
  stanc.check_model({}, basic_model);
} catch (e) {
  console.log(e.message);
}
