var stanc = require('../../src/stancjs/stancjs.bc.js');

var version = stanc.stanc("version-test", "", "--version");
console.log(version.result)

var basic_model = `
parameters {
	real y;
}
model {
    y ~ std_normal();
}
`

var version = stanc.stanc("version-test", basic_model, "--version");
console.log(version.result)