/* $ stanc --auto-format --include-paths=.. ../main2.stan : [exit 0] */
functions {
  void foo(data array[,] real x) {
    print(x);
    
    return;
  }
  
  #include first.stanfunctions
}

