functions {
    // no forward declarations
    real foo(real baz) {
        return bar(baz);
    }
    real bar(real bar) {
        return foo(bar);
    }
    real baz(real foo) {
        return baz(foo);
    }
}
model {
    
}