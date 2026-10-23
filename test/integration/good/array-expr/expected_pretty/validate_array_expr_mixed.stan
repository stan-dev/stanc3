/* $ stanc --auto-format ../validate_array_expr_mixed.stan : [exit 0] */
transformed parameters {
  array[3, 3] real td_arr33 = {{1, 2, 3}, {1, 2., 3}, {1., 2., 3}};
}

