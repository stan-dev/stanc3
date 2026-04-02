functions {
    void foo_rng(real x){
        print(normal_rng(0,x));
    }
}

model {
    foo_rng(1.0);
}
