var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let info_model = `
data {
    int a;
    real b;
    vector[2] c;
    row_vector[3] d;
    matrix[2,2] e;
    array[5] int f;
    array[6] real g;
    array[7] vector[1] h;
    array[2] matrix[2,2] i;
    array[3,1,3] int j;
}
transformed data {
    int k = a + 1;
}
parameters {
    simplex[10] l;
    unit_vector[11] m;
    ordered[12] n;
    positive_ordered[13] o;
    cov_matrix[14] p;
    corr_matrix[15] q;
    cholesky_factor_cov[16] r;
    cholesky_factor_corr[17] s;
    real y;
}
`

let info = stanc.stanc("basic_model", info_model, ["info"]);
utils.print_result(info)
