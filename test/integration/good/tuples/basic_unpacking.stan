transformed data {
  int x = 3;
  int y = 4;
  print(x, y);
  (x, y) = (5, 6);
  print(x, y);
  (x, y) = (y, x);
  print(x, y);
  (/* comment 1*/x/*comment 2*/,/*comment 3*/y/* comment 4 */)/*comment 5*/ = /*comment 6*/ (/*comment 7*/y/*comment 8*/, /*comment 9*/x/*comment 10*/);
  (//comment 1
  x //comment 2
  , //comment 3
  y //comment 4
   ) // comment 5
    =
    // comment 6
     ( // comment 7
      y // comment 8
      , // comment 9
      x // comment 10
      );
}
