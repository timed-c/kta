

#include <stdio.h>


unsigned exper(unsigned len){
  unsigned count = 0;
  int i,j;
  for(i = 0; i < len; i++){
    for(j = 0; j < i; j++){
      count = i + j + count;
    }
  }
  return count;
}


int main(){
  unsigned v = exper(1000);  //Expected outout: exper(300) = 13410150 = 0xCC9F66
    printf("%d\n",v);
}
