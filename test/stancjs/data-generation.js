var stanc = require('../../src/stancjs/stancjs.bc.js');
var utils = require("./utils/utils.js");

let datagen_model = `
data {
    int x[3, 4];
    int y[5, 2, 4];
    matrix[3, 4] z;
    vector[3] w;
    vector[3] p[4];
}
`

let datagen = stanc.stanc("basic_model", datagen_model, ["debug-generate-data"]);
utils.print_result(datagen)
