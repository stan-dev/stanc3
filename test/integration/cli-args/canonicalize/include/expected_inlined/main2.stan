/* $ stanc --auto-format --include-paths=.. --canonicalize=includes ../main2.stan : [exit 0] */
functions {
  void foo(data array[,] real x) {
    print(x);
    
    return;
  }
  
  real bar(matrix x) {
    print(x);
    return sum(x);
  }
}

