functions {
    real test(real k) {
        if (k < 0)
            ;
        reject("test");

        return k;
    }
}
