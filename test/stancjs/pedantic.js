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

var pedantic_test = stanc.stanc("pedantic", pedantic_model);

var warn_uninit_model = `
transformed data { 
    real tt;
    tt = tt + 2;
}
`
var warn_uninit_test = stanc.stanc("uninit", warn_uninit_model, ["warn-uninitialized"]);

var warn_uninit_test = stanc.stanc("uninit", warn_uninit_model);
