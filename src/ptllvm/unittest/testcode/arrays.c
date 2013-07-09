
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


int simple_matrix_access(int x, int y, int x2, int y2, int k)
{
  int m[8][12];
  m[x][y] = k;
  return m[x2][y2];
}


int idx(int* v, int n)
{
  return v[n];
}


int less_simple_matrix_access(int x, int y, int x2, int y2, int k)
{
  int m[8][12];
  int* pm[20];
  int* p; 
  int s;
  m[x][y] = k;
  m[x][y+1] = k+5;
  p = m[x2];
  pm[x] = m[x2];
  s = p[y2] + idx(pm[x2],y2+1);
  return s;
}


int main()
{
  printf("addnums(7) = %d\n", addnums(7));
  printf("simple_array_access(3,3) = %d\n", simple_array_access(3,3));
  printf("simple_matrix_access(4,5,4,5,10) = %d\n", simple_matrix_access(4,5,4,5,10));
  printf("less_simple_matrix_access(4,5,4,5,10) = %d\n", 
         less_simple_matrix_access(4,4,4,4,10));
  return 0;
}
