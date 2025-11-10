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
utils.print_warnings(deprecated_test)


var tc_warn = `
parameters {
    real y;
}
model {
    y ~ normal(0,1);
}
generated quantities {
    int x, w;
   int z = x / w;
}
`
var typechecker_test = stanc.stanc("deprecated", tc_warn);
utils.print_warnings(typechecker_test)

var tc_no_warn = `
parameters {
    real y;
}
model {
    y ~ normal(0,1);
}
`
var typechecker_test2 = stanc.stanc("deprecated", tc_no_warn);
console.assert(typechecker_test2.warnings.length == 0, "Typechecker remembered a warning!")
