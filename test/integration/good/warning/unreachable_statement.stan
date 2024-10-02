functions {
   void foo(real x){
     if(x == 0.0) ; // bug - misplaced ; makes the next statement unconditional
       reject("Cannot be 0");
     return;
   }

  real bar(real x){
    fatal_error("don't call this function");
    return x ;
  }
}
