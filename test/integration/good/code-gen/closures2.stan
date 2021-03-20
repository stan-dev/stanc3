transformed data {
    function
    real foo(real x) {
        return x;
    }
    function
    real bar(real y) {
        return foo(y);
    }
}
parameters {
    real alpha;
}
transformed parameters {
    function
    real baz(real y) {
        return foo(y);
    }
    function
    real goo(real s) {
        function
        real gar(real b) {
            return b;
        }
        return gar(s);
    }
    real s1 = bar(1.0);
    real s2 = baz(1.0);
    real s3 = goo(1.0);
}