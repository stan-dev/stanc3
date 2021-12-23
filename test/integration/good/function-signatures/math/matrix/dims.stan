data {
  int x1x;
  real x2x;
  vector[2] x3x;
  row_vector[2] x4x;
  matrix[2, 3] x5x;
  
  array[3] int x1y;
  array[3] real x2y;
  array[3] vector[2] x3y;
  array[3] row_vector[2] x4y;
  array[3] matrix[2, 3] x5y;
  
  array[3, 4] int x1z;
  array[3, 4] real x2z;
  array[3, 4] vector[2] x3z;
  array[3, 4] row_vector[2] x4z;
  array[3, 4] matrix[2, 3] x5z;
  
  array[3, 4, 5] int x1w;
  array[3, 4, 5] real x2w;
  array[3, 4, 5] vector[2] x3w;
  array[3, 4, 5] row_vector[2] x4w;
  array[3, 4, 5] matrix[2, 3] x5w;
}
transformed data {
  array[0] int val0;
  array[1] int val1;
  array[2] int val2;
  array[3] int val3;
  array[4] int val4;
  array[5] int val5;
  array[6] int val6;
  array[7] int val7;
  array[8] int val8;
  array[9] int val9;
  array[10] int val10;
  
  val0 = dims(x1x);
  val0 = dims(x2x);
  val1 = dims(x3x);
  val1 = dims(x4x);
  val2 = dims(x5x);
  
  val1 = dims(x1y);
  val1 = dims(x2y);
  val2 = dims(x3y);
  val2 = dims(x4y);
  val3 = dims(x5y);
  
  val2 = dims(x1z);
  val2 = dims(x2z);
  val3 = dims(x3z);
  val3 = dims(x4z);
  val4 = dims(x5z);
  
  val3 = dims(x1w);
  val3 = dims(x2w);
  val4 = dims(x3w);
  val4 = dims(x4w);
  val5 = dims(x5w);
}
parameters {
  real y;
  
  real p_x1x;
  real p_x2x;
  vector[2] p_x3x;
  row_vector[2] p_x4x;
  matrix[2, 3] p_x5x;
  
  array[3] real p_x1y;
  array[3] real p_x2y;
  array[3] vector[2] p_x3y;
  array[3] row_vector[2] p_x4y;
  array[3] matrix[2, 3] p_x5y;
  
  array[3, 4] real p_x1z;
  array[3, 4] real p_x2z;
  array[3, 4] vector[2] p_x3z;
  array[3, 4] row_vector[2] p_x4z;
  array[3, 4] matrix[2, 3] p_x5z;
  
  array[3, 4, 5] real p_x1w;
  array[3, 4, 5] real p_x2w;
  array[3, 4, 5] vector[2] p_x3w;
  array[3, 4, 5] row_vector[2] p_x4w;
  array[3, 4, 5] matrix[2, 3] p_x5w;
}
transformed parameters {
  array[0] real p_val0;
  array[1] real p_val1;
  array[2] real p_val2;
  array[3] real p_val3;
  array[4] real p_val4;
  array[5] real p_val5;
  array[6] real p_val6;
  array[7] real p_val7;
  array[8] real p_val8;
  array[9] real p_val9;
  array[10] real p_val10;
  
  p_val0 = dims(x1x);
  p_val0 = dims(x2x);
  p_val1 = dims(x3x);
  p_val1 = dims(x4x);
  p_val2 = dims(x5x);
  
  p_val1 = dims(x1y);
  p_val1 = dims(x2y);
  p_val2 = dims(x3y);
  p_val2 = dims(x4y);
  p_val3 = dims(x5y);
  
  p_val2 = dims(x1z);
  p_val2 = dims(x2z);
  p_val3 = dims(x3z);
  p_val3 = dims(x4z);
  p_val4 = dims(x5z);
  
  p_val3 = dims(x1w);
  p_val3 = dims(x2w);
  p_val4 = dims(x3w);
  p_val4 = dims(x4w);
  p_val5 = dims(x5w);
  
  p_val0 = dims(p_x1x);
  p_val0 = dims(p_x2x);
  p_val1 = dims(p_x3x);
  p_val1 = dims(p_x4x);
  p_val2 = dims(p_x5x);
  
  p_val1 = dims(p_x1y);
  p_val1 = dims(p_x2y);
  p_val2 = dims(p_x3y);
  p_val2 = dims(p_x4y);
  p_val3 = dims(p_x5y);
  
  p_val2 = dims(p_x1z);
  p_val2 = dims(p_x2z);
  p_val3 = dims(p_x3z);
  p_val3 = dims(p_x4z);
  p_val4 = dims(p_x5z);
  
  p_val3 = dims(p_x1w);
  p_val3 = dims(p_x2w);
  p_val4 = dims(p_x3w);
  p_val4 = dims(p_x4w);
  p_val5 = dims(p_x5w);
}
model {
  y ~ normal(0, 1);
}

