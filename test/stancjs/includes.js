var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var include_model = `
#include <foo.stan>
data {
    int a;
}
`

// will fail
var include_test_fails = stanc.stanc("include-testtest", include_model, [], {"bar.stan":"// nothing here"});
utils.print_error(include_test_fails)

var includes = {
    "foo.stan" : `
functions {
    int foo(real a) {
        return 1;
    }
}`};

var include_test = stanc.stanc("include-testtest", include_model, ["auto-format", "canonicalize=includes"], includes);
utils.print_error(include_test)
utils.print_result(include_test)
