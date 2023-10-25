transformed data {
  int x = 3;
  int y = 4;
  print(x, y);
  (x, y) = (5, 6);
  print(x, y);
  (x, y) = (y, x);
  print(x, y);
  (/* comment 1*/x/*comment 2*/,/*comment 3*/y/* comment 4 */)/*comment 5*/ = (y, x);
  (//comment 1
  x //comment 2
  , //comment 3
  y //comment 4
   ) // comment 5
    = (y, x);
}
