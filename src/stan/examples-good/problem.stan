functions {
  int foo(int x);
  
  int bar(int x);
  
  int foo(int x) {
    if (x==0) {return 1;}
    return bar(x -1);
  }
  
  
  int bar(int x) {
    if (x==0) {return 1;}
    return foo(x - 1);
  }
}