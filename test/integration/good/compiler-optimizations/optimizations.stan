functions {
    void nrfun_lp(real x, int y) {
        if (x > 342)
          return
        target += y;
    }

    int rfun(int y) {
        if (y > 2)
            return y + 24;
        return y + 2;
    }

    int rfun_lp() {
        target += 2;
        return 24;
    }
}
parameters {
    real theta;
}
model {
    real x;
    nrfun_lp(4, 3);
    print(rfun(3));
    if (rfun(4)) print("a");
    for (rfun(7) : rfun(5)) {
      target += rfun(8);
      nrfun_lp(34);
    }
    for (i in 1 : 5) {
        target += 53;
    }
    for (i in 1 : 5) {
        if (i > 4)
          break;
        target += 2;
    }
    for (i in 1 : 5) {
        if (i > 4)
          continue;
        target += 2;
    }
    for (i in 1 : 5) {
        if (i > 4)
          continue;
        target += 2;
    }
    x = 3;
    target += x;
    if theta > 2
      x = 2;
    target += x;
    x = 24;
    x = 247;
    target += x;
    x = 24 * 24;
    target += x;
    if theta > 46
      x = 24 * 245;
    target += x;
    for (i in 14 : 35)
      { }
    target += 2;
    {
        real y = 2;
        y = 24;
        target += y;
    }
    {
        real y = 22;
        y = 245;
        target += y;
    }
    if 0 target += 235;
    if 1 target += 2;
    if 24 * 2 { } else { }
    if 24 * 2 ; else ;
    if 24 * 2 { }
    if 20 * 2 ;
    if rfun_lp() ; else ;
    while 0 { target += 325; }
    while 24 * 24 break;
    while rfun_lp() break;
    for (i in 31 : 225)
      continue;
    for (i in 31 : 225)
      break;
    for (i in 31 : 225)
      ;
    for (i in rfun_lp() : 225)
      continue;
    for (i in rfun_lp() : 225)
      break;
    for (i in rfun_lp() : 225)
      ;
    {
        target += 1;
        ;
        ;
        target += 24;
    }
    {
        {
            target += 1;
            ;
        }
        ;
        {
            ;
        }
        {}
    }
}