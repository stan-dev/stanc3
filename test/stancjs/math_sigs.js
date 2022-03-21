var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let stan_math_sigs = stanc.dump_stan_math_signatures();
console.assert(stan_math_sigs.includes("bernoulli_cdf(int, real) => real"), "Failed to find bernoulli signature!")
console.assert(stan_math_sigs.includes("zeros_array(int) => array[] real"), "Failed to find zeros_array signature!")


let stan_math_dists = stanc.dump_stan_math_distributions();
console.assert(stan_math_dists.includes("normal: lpdf, rng, ccdf, cdf"), "Failed to find normal distribution!")
