data { array[5] real a; } model { for (n in a[1]:5) a[n] = n; }
