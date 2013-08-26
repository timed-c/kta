

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

int logic2(unsigned int x, unsigned int y, int z){
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

/*
int comp2(int x, int y, int z){
  int a = x > 4 ? y : z;
  return a;
}
*/

int compare1(unsigned int x, unsigned int y){
  int a = x == y;
  int b = x != y;
  int c = x > y;
  int d = x >= y;
  int e = x < y;
  int f = x <= y;
  return a+b+c+d+e+f;
}

int compare2(int x, int y){
  int a = x == y;
  int b = x != y;
  int c = x > y;
  int d = x >= y;
  int e = x < y;
  int f = x <= y;
  return a+b+c+d+e+f;
}

int compare3(int x, int y){
  int a = x == 0;
  int b = x != 0;
  int c = x > 200;
  int d = x >= 1240;
  int e = x < -1234;
  int f = x <= 4252;
  return a+b+c+d+e+f;
}

/*
  | TExp(ICmp(id,IcmpEq,ty,_,_),[e1;e2])  -> not_imp "IcmpEq"
  | TExp(ICmp(id,IcmpNe,ty,_,_),[e1;e2])  -> not_imp "IcmpNe" 
  | TExp(ICmp(id,IcmpUgt,ty,_,_),[e1;e2]) -> not_imp "IcmpUgt"
  | TExp(ICmp(id,IcmpUge,ty,_,_),[e1;e2]) -> not_imp "IcmpUge"
  | TExp(ICmp(id,IcmpUlt,ty,_,_),[e1;e2]) -> not_imp "IcmpUlt"
  | TExp(ICmp(id,IcmpUle,ty,_,_),[e1;e2]) -> not_imp "IcmpUle"
  | TExp(ICmp(id,IcmpSgt,ty,_,_),[e1;e2]) -> not_imp "IcmpSgt"
  | TExp(ICmp(id,IcmpSge,ty,_,_),[e1;e2]) -> not_imp "IcmpSge"
  | TExp(ICmp(id,IcmpSlt,ty,_,_),[e1;e2]) -> not_imp "IcmpSlt"
  | TExp(ICmp(id,IcmpSle,ty,_,_),[e1;e2]) -> not_imp "IcmpSle"        
*/

int main(){
  printf("arith1() = %d\n", arith1(12,52,3));
  printf("arith2() = %d\n", arith2(721,888,7));
  printf("logic1() = %d\n", logic1(4,0,9));
  printf("logic2() = %d\n", logic2(8,2,12));
  printf("comp1() = %d\n", comp1(88,12,99));
  return 0;
}
