

#include <stdio.h>


int foo1(int x, int y)
{
  return x + y;
}

int functest1(int k)
{
  return foo1(k, k*k);
}



int main(){

  printf("functest1(77) = %d\n", functest1(77));
  return 0;
}
