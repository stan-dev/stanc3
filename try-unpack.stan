transformed data {
   int x = 3;
   int y = 4;


  (x,y) = (5,6);
  //  (x,y) = (y,x);
}
generated quantities {
   int x_out = x;
    int y_out = y;
}
