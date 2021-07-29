parameters {
   complex x;
}
generated quantities {
   complex y = to_complex(3,4);
   complex z;
   int i = 1;
   real r = 1.0; 

   z = x+y;
   z = x+r;
   z = r+x;
   z = x+i;
   z = i+x;

   z = x-y;
   z = x-r;
   z = r-x;
   z = x-i;
   z = i-x;

   z = x*y;
   z = x*r;
   z = r*x;
   z = x*i;
   z = i*x;

   z = x/y;
   z = x/r;
   z = r/x;
   z = x/i;
   z = i/x;

   z = -x;
   z = -r;
   z = -i;
}
