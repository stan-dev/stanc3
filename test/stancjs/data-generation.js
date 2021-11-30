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
const res = JSON.parse(datagen.result)
console.log("dim(x) = (" + res.x.length + ", " + res.x[0].length + ")")
console.log("dim(y) = (" + res.y.length + ", " + res.y[0].length + ", " + res.y[0][0].length + ")")
console.log("dim(z) = (" + res.z.length + ", " + res.z[0].length + ")")
console.log("dim(w) = (" + res.w.length + ")")
console.log("dim(p) = (" + res.p.length + ", " + res.p[0].length + ")")
console.log()