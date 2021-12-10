functions {
   void foo(real x){
     if(x) ; // bug - misplaced ; makes the next statement unconditional
       reject("Cannot be 0");
     return;
   }
}
