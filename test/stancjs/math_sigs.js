var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let stan_math_sigs = stanc.stanc("sigs", "", ["dump-stan-math-signatures"]);
console.log(stan_math_sigs.result)

