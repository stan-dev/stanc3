model {
  int y = 1;

  print(y == 1 ? 42 : 1 %/% 0);

  if (y == 0 && 1 %/% 0 == 0)
    print("unreachable");

  if (y == 1 || 1 %/% 0 == 0)
    print("reachable");
}
