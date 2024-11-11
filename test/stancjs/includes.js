var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var include_model = `
#include <foo.stan>
data {
    int a;
}
`


// will fail
var include_test_missing = stanc.stanc("include-testtest", include_model, []);
utils.print_error(include_test_missing)

var bar_includes = {"bar.stan":"// nothing here"};
var include_test_missing = stanc.stanc("include-testtest", include_model, [], bar_includes);
utils.print_error(include_test_missing)

var include_test_missing = stanc.stanc("include-testtest", include_model, [], {"./foo.stan":"// not exact filename match"});
utils.print_error(include_test_missing)

// good
var foo_code = `
functions {
    int foo(real a) {
        return 1;
    }
}`;

// vanilla js behavior: last wins
var includes_multi_test = stanc.stanc("include-testtest", include_model, [], { "foo.stan": "syntax error", "foo.stan":foo_code});
utils.print_error(includes_multi_test)
utils.print_warnings(includes_multi_test)

var include_format_test = stanc.stanc("include-testtest", include_model, ["auto-format", "canonicalize=includes"], {"foo.stan":foo_code});
utils.print_error(include_format_test)
utils.print_result(include_format_test)

var include_info_test = stanc.stanc("include-testtest", include_model, ["info"], {...bar_includes, "foo.stan":foo_code});
utils.print_error(include_info_test)
utils.print_result(include_info_test)


// warnings
var include_test_bad = stanc.stanc("empty", "model {}", [], {"foo.stan": {"internal":"that wasn't a string!"}});
utils.print_error(include_test_bad)
utils.print_warnings(include_test_bad)

var include_test_bad = stanc.stanc("empty", "model {}", [], 1234);
utils.print_error(include_test_bad)
utils.print_warnings(include_test_bad)

// other errors: recursive includes
var recursive_a = `// comment here\n#include <include/b.stan>`
var recursive_b = `#include <include/a.stan>\n // comment here`

var recursive_test = stanc.stanc("recursive", recursive_a, ["filename-in-msg=include/a.stan"], {"include/a.stan":recursive_a, "include/b.stan":recursive_b});
utils.print_error(recursive_test)
utils.print_warnings(recursive_test)
