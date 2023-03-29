functions {
  real external_fun(real a);

  vector external_map_rectable(vector phi, vector theta,
                               data array[] real x_r, data array[] int x_i);

  real internal_fun(data array[,] real a, data array[,] int d);
  // forward decl, shouldn't duplicate in output
  real internal_fun(data array[,] real a, data array[,] int d) {
    return sum(map_rect(external_map_rectable, []', {[]'}, a, d));
  }
}
