
#include <stdio.h>

int smallloop1(int k){
  int j = 2;
  int i;
  for(i=1; i< k; i++){
    j = j+i*j;
  }
  return j;
}

int main(){
  int k = 10;
  printf("i=%d for k=%d\n", smallloop1(k), k);
  return 0;
}
