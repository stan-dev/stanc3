parameters {
  array[3] real X;
}

model {
  array[3] int Y = to_int(X);
}
