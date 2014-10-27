

#include "stdio.h"


int foo(int max, int k)
{
  int i;
  int r = 0;
  for(i=0; i < max; i += k){
    r += i;
  }
  return r;
}


int main(){

  printf("Result: %d\n", foo(1000, 3));
  return 0;
}
