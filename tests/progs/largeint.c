

#include <stdio.h>


int main()
{
  long l = 0x123456789;
  printf("Long size %d\nVal %d\n", sizeof(l), l);
  long long ll = 0x123456789;
  printf("Long long size %d\nVal %d\n", sizeof(ll), ll);
  return 0;
}
