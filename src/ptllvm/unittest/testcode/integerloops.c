
#include <stdio.h>

void foo(){
  printf("foo\n");
}

int looptest1(int k){
  int j = 2;
  int i;
  for(i=1; i< k; i++){
    j = j+i;
  }
  return j;
}

int looptest2(int k){
  int j = 2;
  int i;
  for(i=1; i< k; i++){
    j = j+i*j;
  }
  return j;
}

int main(){
  int k = 10;
  printf("i=%d for k=%d\n", looptest1(k), k);
  printf("i=%d for k=%d\n", looptest2(k), k);
  return 0;
}
