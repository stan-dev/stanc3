var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var deprecated_model = `
parameters {
    real y;
    # hash comment is deprecated
}
model {
    y ~ normal(0,1);
}
`
var deprecated_test = stanc.stanc("deprecated", deprecated_model);
console.log(deprecated_test.warnings)