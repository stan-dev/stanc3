var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var pedantic_model = `
parameters {
	real y;
	real k;
}
model {
    y ~ normal(0,10000);
}
`
var pedantic_test = stanc.stanc("pedantic", pedantic_model, ["warn-pedantic"]);
if (pedantic_test.warnings) {
    console.log(JSON.stringify(pedantic_test.warnings))
}

var pedantic_test = stanc.stanc("pedantic", pedantic_model);
if (pedantic_test.warnings) {
    console.log(JSON.stringify(pedantic_test.warnings))
}
var warn_uninit_model = `
transformed data { 
    real tt;
    tt = tt + 2;
}
`
var warn_uninit_test = stanc.stanc("uninit", warn_uninit_model, ["warn-uninitialized"]);
if (warn_uninit_test.warnings) {
    console.log(JSON.stringify(warn_uninit_test.warnings))
}

var warn_uninit_test = stanc.stanc("uninit", warn_uninit_model);
if (warn_uninit_test.warnings) {
    console.log(JSON.stringify(warn_uninit_test.warnings))
}