
#include <stdio.h>

void foo(){
  printf("foo\n");
}

int looptest(int k){
  int j = 2;
  int i;
  for(i=1; i< k; i++){
    j = j+ i;
  }
  return j;
}

int main(){
  int k = 10;
  printf("i=%d for k=%d\n", looptest(k), k);
  return 0;
}
