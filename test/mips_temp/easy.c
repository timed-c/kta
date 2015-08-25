


#include "tpp.h"

int g1;

// To avoid that timing program points are duplicated, we must mark the
// function to not be inlined.
int __attribute__ ((noinline)) foo()
{
  int u = g1 * 100;
  int i;

  TPP(1);

  for(i=0; i<u; i++)
    u += i;
  TPP(2);
  return u;
}

int main()
{

  g1 = 10;
  foo();
  
  return 0;
}
