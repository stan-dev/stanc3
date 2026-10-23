/* $ stanc --auto-format ../break-continue.stan : [exit 0] */
// test right value passed through for, while and embedded
functions {
  int foo(int a) {
    // direct while
    while (1) break;
    while (0) continue;
    
    // direct for
    for (i in 1 : 10) 
      break;
    for (i in 1 : 10) 
      continue;
    
    // in statement seq
    while (1) {
      int b;
      b = 5;
      break;
    }
    
    // if, else if, else body
    while (1) {
      if (0) 
        break;
      else if (1) 
        break;
      else 
        break;
    }
    
    // nested while
    while (1) while (0) break;
    
    // nested for
    while (1) {
      for (i in 1 : 10) 
        break;
    }
    
    // nested foreach (array)
    while (1) {
      array[2, 3] int vs;
      int z;
      for (v in vs) {
        z = 0;
        break;
      }
      for (v in vs) {
        z = 0;
        continue;
      }
      for (v in vs) {
        for (vv in v) {
          z = 0;
          break;
        }
        z = 1;
      }
    }
    
    // nested foreach (matrix)
    while (1) {
      real z;
      matrix[2, 3] vs;
      for (v in vs) {
        z = 0;
        break;
      }
      for (v in vs) {
        z = 3.2;
        continue;
      }
    }
    
    // nested foreach (vector)
    while (1) {
      real z;
      vector[2] vs;
      for (v in vs) {
        z = 0;
        break;
      }
      for (v in vs) {
        z = 3.2;
        continue;
      }
    }
    
    // nested foreach (rowvector)
    while (1) {
      real z;
      row_vector[2] vs;
      for (v in vs) {
        z = 0;
        break;
      }
      for (v in vs) {
        z = 3.2;
        continue;
      }
    }
    
    // nested block
    while (1) {
      int b;
      b = 5;
      {
        int c;
        c = 6;
        break;
      }
    }
    
    // nested loop exit
    while (1) {
      while (1) {
        
      }
      break;
    }
    while (1) {
      while (1) {
        
      }
      continue;
    }
    
    return 0;
  }
}
transformed data {
  int x;
  x = 0;
  while (0) break;
  while (1) continue;
}
parameters {
  real y;
}
transformed parameters {
  real z;
  z = 1;
  while (0) break;
  while (1) continue;
}
model {
  
}
generated quantities {
  real u;
  u = 1;
  while (1) break;
  while (0) continue;
}

/*
warning: Unreachable statement found, is this intended?
    ┌─ ../break-continue.stan:119:7
115 │      while (1) {
116 │        while (1) {
    │               - endless loop
117 │          
118 │        }
119 │        break;
    │        ^ Never reached
120 │      }
warning: Unreachable statement found, is this intended?
    ┌─ ../break-continue.stan:125:7
121 │      while (1) {
122 │        while (1) {
    │               - endless loop
123 │          
124 │        }
125 │        continue;
    │        ^ Never reached
126 │      }
warning: Unreachable statement found, is this intended?
    ┌─ ../break-continue.stan:35:5
 31 │      // nested while
 32 │      while (1) while (0) break;
    │             - endless loop
 33 │      
 34 │      // nested for
 35 │      while (1) {
    │      ^ Never reached
 36 │        for (i in 1 : 10) 
*/
