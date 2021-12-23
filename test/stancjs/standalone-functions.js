var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let basic_stanfuncs = `
functions {
    real my_log1p_exp(real x) {
        return log1p_exp(x);
    }
	
	real array_fun(real[] a)
	{
		return sum(a);
	}
  
	real int_array_fun(int[] a)
	{
		return sum(a);
	}

	vector my_vector_mul_by_5(vector x)
	{
		vector[num_elements(x)] result = x * 5.0;
		return result;
	}

	int int_only_multiplication(int a, int b) {
		return a*b;
	}

	real test_lgamma(real x) {
        return lgamma(x);
    }

	// test special functions
	void test_lp(real a) {
    	a ~ normal(0, 1);
	}

	real test_rng(real a) {
		return normal_rng(a, 1);
	}

	real test_lpdf(real a, real b) {
		return normal_lpdf(a | b, 1);
	}
}
`

let basic = stanc.stanc("basic_stanfuncs", basic_stanfuncs, ["standalone-functions"]);
utils.print_error(basic)
var ind = basic.result.search("auto int_only_multiplication");
if (ind == -1) {
	console.log("ERROR: standalone-functions!")
}
