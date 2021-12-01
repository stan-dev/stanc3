functions {
    real foo1(real a, int b, real[] c, vector d, matrix e, row_vector f) {
       return 5.0;    
    }
    vector foo2(vector a, matrix[] b, row_vector[] c) {
       return a;
    }
    matrix foo3(vector a, matrix b) {
       return b;
    }
    matrix add_udf(matrix a, matrix b) {
       return add(a, b);
    }
}
data {
    int N;
    matrix[N,N] m;
}