functions {
    void nrfun_lp(real x, int y) {
        if (x > 342)
          return;
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
    real phi;
    matrix[3, 2] x_matrix;
    vector[2] x_vector;
    cov_matrix[2] x_cov;
}
model {
    real x;
    nrfun_lp(4, 3);
    print(rfun(3));
    if (rfun(4)) print("a");
    for (i in rfun(7) : rfun(5)) {
      target += rfun(8);
      nrfun_lp(34, 3);
    }
    for (i in 1 : 5) {
        target += 53;
    }
    for (i in 1 : 5)
        for (j in i : i+2)
            for (k in j : j*2)
                target += 53;
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
    if (theta > 2)
      x = 2;
    target += x;
    x = 24;
    x = 247;
    target += x;
    x = 24 * 24;
    target += x;
    if (theta > 46)
      x = 24 * 245;
    target += x;
    real z;
    z = x;
    target += z;
    if (theta > 46)
      z = x;
    target += z;
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
    if (0) target += 235;
    if (1) target += 2;
    if (24 * 2) { } else { }
    if (24 * 2) ; else ;
    if (24 * 2) { }
    if (20 * 2) ;
    if (rfun_lp()) ; else ;
    while (0) { target += 325; }
    while (24 * 24) break;
    while (rfun_lp()) break;
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
    real temp;
    if (2 > 3)
      temp = 2 * 2;
    else
      print("hello");
    temp = 2 * 2;
    real temp2;
    for (i in 2 : 3) {
        temp2 = 2 * 3;
        target += temp;
        target += temp2;
    }
    real dataonlyvar = 3;
    if (3 > 4)
      dataonlyvar = 3 * 53;
    target += dataonlyvar;
    real paramvar = 3;
    if (42 > 1)
      paramvar = theta * 34;
    target += paramvar;
}