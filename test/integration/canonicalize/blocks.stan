data {
  int N;
  vector[N] y;
}
model {
    int j = 0;

    // test for - with / without parens
    for (i in 1:N) target += i;
    for (i in 1:N) {target += i;}

    // test for - with / without parens
    while(j<N) j = j+1;
    while(j<N) {j = j+1;}
    while((j<N)) j = j+1;

    // test if - with / without parens
    if (N>5) y ~ std_normal();
    if (N>5) {y ~ std_normal();}

    // test if-else - with / without parens
    if (N>5)
      y ~ std_normal();
    else
      y ~ std_normal();

    if (N>5)
      {y ~ std_normal();}
      else
      y ~ std_normal();

    if (N>5)
      {y ~ std_normal();
      } else
      y ~ std_normal();

    if (N>5)
      y ~ std_normal();
    else
      {y ~ std_normal();}

    if (N>5)
      {y ~ std_normal();}
    else
      {y ~ std_normal();}

    ///////////////////////////////
    // nested if else combinations
    ///////////////////////////////
    if (N>5) y ~ std_normal();
    else if (N>5) y ~ std_normal();

    if (N>5) y ~ std_normal();
    else if (N>5) y ~ std_normal();

    if (N>5) if (N>5) y ~ std_normal();
    else y ~ std_normal();

    if (N>5) {if (N>5) y ~ std_normal();}
    else y ~ std_normal();

    if (N>5) y ~ std_normal();
    else if (N>5) y ~ std_normal();
    else if (N>5) y ~ std_normal();

    if (N>5) y ~ std_normal();
    else if (N>5) y ~ std_normal();
    else {if (N>5) y ~ std_normal();}

    if (N>5) {y ~ std_normal();}
    else {if (N>5) y ~ std_normal();
    else if (N>5) y ~ std_normal();}


    // test comments in various places wrt IfThenElse
    if (N>5) //comment before then branch
      y ~ std_normal();
    else y ~ std_normal();

    if (N>5) y ~ std_normal(); //comment after then branch
    else y ~ std_normal();

    if (N>5) y ~ std_normal();
    else //comment before else branch
    y ~ std_normal();

    if (N>5) y ~ std_normal();
    else
    y ~ std_normal();  //comment after else branch
}
