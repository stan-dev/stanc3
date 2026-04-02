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

let basic_name1 = stanc.stanc("basic1", basic_model, []);
utils.print_error(basic_name1);
console.assert(basic_name1.result.includes("namespace basic1_namespace"), "Error: namespace not in C++ code");

// test that the state doesn't persist

let basic_name2 = stanc.stanc("basic2", basic_model, []);
utils.print_error(basic_name2);
console.assert(basic_name2.result.includes("namespace basic2_namespace"), "Error: namespace not in C++ code");


// test that _model is added if the name is file-like

let basic_name3 = stanc.stanc("basic.stan", basic_model, []);
utils.print_error(basic_name3);
console.assert(basic_name3.result.includes("namespace basic_model_namespace"), "Error: namespace not in C++ code");
