var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");



let bad = `
parameters {
	real y;
}
model {
    y ~ std_normal();
}
model {
    y ~ std_normal();
}
`

let basic_bad = stanc.stanc("basic_bad", bad, []);
utils.print_error(basic_bad)

// we now test a syntactically valid model to make sure the parser has cleared its state

let basic_model = `
parameters {
	real y;
}
model {
    y ~ std_normal();
}
`

let basic = stanc.stanc("basic", basic_model);
utils.print_error(basic)
