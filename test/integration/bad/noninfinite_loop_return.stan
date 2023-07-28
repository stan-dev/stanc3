functions {
  int bin_search(real x, int min_val, int max_val) {
    if (min_val > x || max_val < x)
      reject("require min < x < max, found min = ", min_val, "; max = ",
             max_val, "; x = ", x);
    real y = round(x);
    int range = max_val - min_val;
    int mid_pt = min_val;
    while (00_00) {
      if (range == 0)
        return mid_pt;
      range = (range + 1) %/% 2;
      mid_pt += y > mid_pt ? range : -range;
    }
  }
}
transformed data {
  int N = bin_search(34.5, 0, 50);
  print(N);
}
