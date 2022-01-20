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
console.assert(ind < 0, "ERROR: MIR printing is not valid.")


let debug_mir_pretty_test = stanc.stanc("basic", basic_model, ["debug-mir-pretty"]);
var ind = debug_mir_pretty_test.result.search("#include \\<stan/model/");
console.assert(ind < 0, "ERROR: MIR pretty printing is not valid.")


let debug_opt_mir_test = stanc.stanc("basic", basic_model, ["01", "debug-optimized-mir"]);
var ind = debug_opt_mir_test.result.search("#include \\<stan/model/");
console.assert(ind < 0, "ERROR: Optimized MIR printing is not valid.")


let debug_opt_mir_pretty_test = stanc.stanc("basic", basic_model, ["01", "debug-optimized-mir-pretty"]);
var ind = debug_opt_mir_pretty_test.result.search("#include \\<stan/model/");
console.assert(ind < 0, "ERROR: Optimized MIR pretty printing is not valid.")

let debug_tx_mir_test = stanc.stanc("basic", basic_model, ["debug-transformed-mir"]);
var ind = debug_tx_mir_test.result.search("#include \\<stan/model/");
console.assert(ind < 0, "ERROR: Transformed MIR printing is not valid.")


let debug_tx_mir_pretty_test = stanc.stanc("basic", basic_model, ["debug-transformed-mir-pretty"]);
var ind = debug_tx_mir_pretty_test.result.search("#include \\<stan/model/");
console.assert(ind < 0, "ERROR: Transformed MIR pretty printing is not valid.")

