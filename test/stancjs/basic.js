var stanc = require('../../src/stancjs/stancjs.bc.js');

var basic_model = `
parameters {
	real y;
}
model {
    y ~ std_normal();
}
`

var version = stanc.stanc("basic", basic_model, "");
console.log(version.result)