
//#define PRINT_MAIN

#ifdef PRINT_MAIN
#include <stdio.h>
#endif

int k;
int v = 1;

/*
int foo(int x, int y){
  return x + y * 10;
}
*/

/*
// Tests: slt,bne,sll,jr,mul,addiu
int foo(int x, int y){
  int z = x * y;
  if(x > y)
    return x;
  else
    return z;
}
*/

/*
// Tests: blez, subu, sll, addiu, bne
int foo(int x){
  int i;
  int r = 23;
  for(i=0; i < x; i++){
    r += (i+100) * 7;
  }  
  return r;
}
*/


/*
// TODO: FIx, not supportet yet
// mult, mfhi
int foo(int x){
  int i;
  int r = 23;
  for(i=0; i < x; i++){
    r += (i+100) % 7;
  }  
  return r;
}
*/


//strange asm using xor and no branch
int foo(int x, int y){
  int z = x * y;
  if(x == y)
    return x;
  else
    return z;
}


int main()
{
  #ifdef PRINT_MAIN
  int x = foo(7,7);
  printf("Result: 0x%x  %d\n", x, x);
  #endif
  return 0;
}
