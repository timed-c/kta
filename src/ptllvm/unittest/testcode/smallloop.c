
#include <stdio.h>

/*
int foo(int x){
  return 2 * x;
}
*/
extern long foo(char*,int,int) asm("llvm.pret_foo");
extern void none() asm("llvm.pret_none");

int main(){
  int j = 2;
  int i;
  for(i=1; i< 10; i++){
    j = j+ i;
    none();
    printf("%d\n", (int)foo("hej",j,j));
  }
  return j;
}
