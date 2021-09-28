transformed data {
    function
    real foo(real x) {
        return x;
    }
    function
    real capture_dataonly_closure(real y) {
        return foo(y);
    }
}
parameters {
    real alpha;
}
transformed parameters {
    function
    real capture_closure(real y) {
        return foo(y);
    }
    function
    real closure_in_closure(real s) {
        function
        real gar(real b) {
            return b;
        }
        return gar(s);
    }
    real s1 = capture_dataonly_closure(1.0);
    real s2 = capture_closure(1.0);
    real s3 = closure_in_closure(1.0);
}