


#include "tpp.h"

int g1 = 1;
int g2 = 7;
//int g3;

// To avoid that timing program points are duplicated, we must mark the
// function to not be inlined.
int __attribute__ ((noinline)) foo()
{
  int u = g1 * 10;
  int i;

  TPP(1);

  for(i=0; i<g1; i++)
    u += i * g2;
  TPP(2);
  return u;
}

int main()
{

  
  foo();
  
  return 0;
}
