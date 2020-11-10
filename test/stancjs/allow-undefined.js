var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var undef_model = `
functions {
    int foo(real a);
}
transformed data {
    real p = foo(5.0);
}
`
var undef_test = stanc.stanc("undefined-test", undef_model, ["allow-undefined"]);
utils.print_error(undef_test)

var undef_test = stanc.stanc("undefined-test", undef_model, []);
utils.print_error(undef_test)
