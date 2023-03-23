var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var syntax = `
functions {
    int foo(real a) {;
}
`
var syntax_test = stanc.stanc("undefined-test", syntax, []);
utils.print_error(syntax_test)


var type_err = `
functions {
    int foo(real a) {
        return 3.5;
    }
}
`
var type_err = stanc.stanc("undefined-test", type_err, []);
utils.print_error(type_err)
