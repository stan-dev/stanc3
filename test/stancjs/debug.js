var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let basic_model = `
parameters {
real y;
}
model {
y~std_normal();
}
`

let debug_mir_test = stanc.stanc("basic", basic_model, ["debug-mir"]);
var ind = debug_mir_test.result.search("#include \\<stan/model/");
if (ind > -1) {
    console.log("ERROR: MIR printing is not valid.")
}

let debug_opt_mir_test = stanc.stanc("basic", basic_model, ["debug-optimized-mir"]);
var ind = debug_opt_mir_test.result.search("#include \\<stan/model/");
if (ind > -1) {
    console.log("ERROR: Optimized MIR printing is not valid.")
}

let debug_tx_mir_test = stanc.stanc("basic", basic_model, ["debug-transformed-mir"]);
var ind = debug_tx_mir_test.result.search("#include \\<stan/model/");
if (ind > -1) {
    console.log("ERROR: Transformed MIR printing is not valid.")
}
