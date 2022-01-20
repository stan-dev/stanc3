functions {
   real bar(array[] real xs){
     return 1.0;
   }


   real bar(array[] complex xs){
     return 2.0;
   }
}

model {
   print(bar({1,2}));
   print(bar({2.3, 3}));
}
