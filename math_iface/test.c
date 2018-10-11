#include <stdio.h>

extern double lognormal_ccdf_log(double, double, double);

int main()
{
   // printf() displays the string inside quotation
   printf("Hello, World!");
   printf("%f", lognormal_ccdf_log(1,1,1));
   return 0;
}
