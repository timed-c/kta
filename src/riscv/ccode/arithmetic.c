

#include <stdio.h>

int arith1(int x, int y, int z){
  int a = x*x*x*23 / y - y*y;
  int b = ((a << z) + y ) >> (z - 1);
  int c = b % y;
  return c;
}

int arith2(unsigned int x, unsigned int y, unsigned int z){
  unsigned int a = (x * x + y) / z;
  unsigned int b = (x * x - y) % z;
  return a + b;
}

int logic1(int x, int y, int z){
  int a = x & y;
  int b = x && y && z;
  return a + b;
}


int main(){
  printf("arith1() = %d\n", arith1(12,52,3));
  printf("arith2() = %d\n", arith2(721,888,7));
  printf("logic2() = %d\n", arith2(4,0,9));
  return 0;
}
