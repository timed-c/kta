

#include <stdio.h>


int simple_switch(int k)
{
  int x;
  switch(k)
    {
    case 77 :
      x = 93;
      break;
    case 19 :
      x = 12 + k;                              
      break;
    case 222 :
      x = k * k;
      break;
    default :
      x = 8;
    }
  return x * 2;
}

int main()
{
  printf("simple_switch(222) = %d\n", simple_switch(222)); 
  printf("simple_switch(93) = %d\n", simple_switch(93)); 
  return 0;
}
