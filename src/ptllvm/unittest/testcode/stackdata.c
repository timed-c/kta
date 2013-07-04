

#include <stdio.h>



int addnums(int k)
{
  int a[10];
  int i;
  int out = 100;
  for(i=0; i<10; i++)
    a[i] = i + k;
  
  for(i=0; i<10; i++)
    out += a[i];
  return out;
}

int simple_array_access(int k, int n)
{
  int a[10];
  a[k] = k;
  return a[n];
}



int main()
{
  printf("addnums(7) = %d\n", addnums(7));
  printf("simple_array_access(3,3) = %d\n", simple_array_access(3,3));
  return 0;
}
