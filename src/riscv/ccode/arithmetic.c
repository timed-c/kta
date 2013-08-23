

#include <stdio.h>

int arith1(int x, int y, int z){
  int a = x*x*x*23 / y - y*y;
  int b = ((a << z) + y ) >> (z - 1);
  int c = b % y;
  return c;
}



int main(){
  int k = 10;
  printf("arith1() = %d \n", arith1(12,52,3));
  return 0;
}
