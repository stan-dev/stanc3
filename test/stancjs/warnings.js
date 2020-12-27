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
console.log(JSON.stringify(deprecated_test.warnings))

var test_no_stderr_model = `
transformed data {
    real p;
    p = p + 2.0;
    p <- 5.0;
}
parameters {
    real y;
    # hash comment is deprecated
}
model {
    y ~ normal(0,1);
    print(get_lp());
    increment_log_prob(5.0);
}
`
var test_no_stderr = stanc.stanc("no_stderr", test_no_stderr_model, ["warn-uninitialized"]);