var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

var opt_model = `
transformed data { 
    real p = 0;
    for(t in 1:5){
        p = p + 1;
    }
}
`
var opt_test = stanc.stanc("optimization", opt_model, []);
var ind = opt_test.result.search("int t = 1; t <= 5; \\+\\+t");
if (ind == -1) {
    console.log("ERROR: Optimization without the O flag!")
}

var opt_test = stanc.stanc("optimization", opt_model, ["O"]);
var ind = opt_test.result.search("int t = 1; t <= 5; \\+\\+t");
if (ind > -1) {
    console.log("ERROR: No optimization without the O flag!")
}

var glm_model = `
data {
    int<lower=1> k;
    int<lower=0> n;
    matrix[n, k] X;
    int y[n];
  } 
   
  parameters {
    vector[k] beta;
    real alpha;
  } 
  
  model {
    target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
  }
`

var no_opencl_test = stanc.stanc("no_opencl", glm_model);
utils.print_error(no_opencl_test)
var ind = no_opencl_test.result.search("matrix_cl<int> y_opencl__");
if (ind > -1) {
    console.log("ERROR: OpenCL code found without the use-opencl flag!")
}

var opencl_test = stanc.stanc("opencl", glm_model, ["use-opencl"]);
utils.print_error(opencl_test)
var ind = opencl_test.result.search("matrix_cl<int> y_opencl__");
if (ind == -1) {
    console.log("ERROR: No OpenCL code found with the use-opencl flag!")
}

// multiple flags

var glm_model2 = `
functions {
    int foo(real a);
}

data {
    int<lower=1> k;
    int<lower=0> n;
    matrix[n, k] X;
    int y[n];
}

transformed data {
    real p = foo(5.0);
}

parameters {
    vector[k] beta;
    real alpha;
} 

model {
    target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
}
`
var opencl_test = stanc.stanc("opencl", glm_model2, "use-opencl");
utils.print_error(opencl_test)

var opencl_test = stanc.stanc("opencl", glm_model2, ["use-opencl", "allow-undefined"]);
utils.print_error(opencl_test)
var ind = opencl_test.result.search("matrix_cl<int> y_opencl__");
if (ind == -1) {
    console.log("ERROR: No OpenCL code found with the use-opencl flag!")
}
