var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var include_model = `
#include <foo.stan>
data {
    int a;
}
`

var bar_includes = {"bar.stan":"// nothing here"};

// will fail
var include_test_missing = stanc.stanc("include-testtest", include_model, [], bar_includes);
utils.print_error(include_test_missing)


var include_test_bad = stanc.stanc("empty", "model {}", [], {"foo.stan": {"internal":"that wasn't a string!"}});
utils.print_error(include_test_bad)
utils.print_warnings(include_test_bad)

var include_test_bad = stanc.stanc("empty", "model {}", [], {"foo.stan": "1", "./foo.stan": "2"});
utils.print_error(include_test_bad)
utils.print_warnings(include_test_bad)

var include_test_bad = stanc.stanc("empty", "model {}", [], 1234);
utils.print_error(include_test_bad)
utils.print_warnings(include_test_bad)

// good
var foo_code = `
functions {
    int foo(real a) {
        return 1;
    }
}`;

var includes_rel_test = stanc.stanc("include-testtest", include_model, [], {"./foo.stan":foo_code});
utils.print_error(includes_rel_test)

var include_format_test = stanc.stanc("include-testtest", include_model, ["auto-format", "canonicalize=includes"], {"foo.stan":foo_code});
utils.print_error(include_format_test)
utils.print_result(include_format_test)

var include_info_test = stanc.stanc("include-testtest", include_model, ["info"], {...bar_includes, "foo.stan":foo_code});
utils.print_error(include_info_test)
utils.print_result(include_info_test)
