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
utils.print_warnings(pedantic_test)


var pedantic_test = stanc.stanc("pedantic", pedantic_model);
utils.print_warnings(pedantic_test)

var warn_uninit_model = `
transformed data {
    real tt;
    tt = tt + 2;
}
`
var warn_uninit_test = stanc.stanc("uninit", warn_uninit_model, ["warn-uninitialized"]);
utils.print_warnings(warn_uninit_test)


var warn_uninit_test = stanc.stanc("uninit", warn_uninit_model);
utils.print_warnings(warn_uninit_test)
