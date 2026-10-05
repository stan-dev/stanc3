module.exports.print_error = function(m) {
    if (m.errors) {
        for (let i = 0; i < m.errors.length; i++) {
            if (i % 2 == 1) {
                console.log(m.errors[i])
            }
        }
    }
};

module.exports.print_result = function(m) {
    console.log(m.result)
};


module.exports.print_warnings = function(m){
    if (m.warnings){
        for (w of m.warnings){
            console.log(w)
        }
    }
}


module.exports.print_json = function(m){
  console.log(JSON.stringify(m, null, 2))
}
