

#include "stdio.h"


int foo(int max, int a, int b, int k)
{
  int i,j;
  int r = 0;
  for(i=0; i < max; i += k){
    if(i < a)
      for(j=i; j >= 10; j--)
        r += j;
    if(i >= b)
      break;
    r += i;
  }
  return r;
}


int main(){

  printf("Result: %d\n", foo(1000, 100, 900, 3));
  return 0;
}
