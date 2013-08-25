

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

int logic2(int x, unsigned int y, int z){
  int a = x >> y;
  int b = (x & (z | y)) ^ a; 
  return a + b;
}

int comp1(int x, int y, int z){
  int a = x * 0x2120fb;
  int b = y * 0xffffff88;
  int c = z * 2047;
  return a + b + c + 123423235;
}


int main(){
  printf("arith1() = %d\n", arith1(12,52,3));
  printf("arith2() = %d\n", arith2(721,888,7));
  printf("logic1() = %d\n", logic1(4,0,9));
  printf("logic2() = %d\n", logic2(8,2,12));
  printf("comp1() = %d\n", comp1(88,12,99));
  return 0;
}
