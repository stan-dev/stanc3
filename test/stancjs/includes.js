var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var include_model = `
#include <foo.stan>
data {
    int a;
}
`

var includes = {
    "foo.stan" : `
functions {
    int foo(real a) {
        return 1;
    }
}`};

var include_test = stanc.stanc("include-testtest", include_model, [], includes);
utils.print_error(include_test)

