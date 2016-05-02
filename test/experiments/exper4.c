
//#include <stdio.h>

unsigned double2(unsigned x){
  return x + x;
}


unsigned exper(unsigned len){
  unsigned count = 0;
  int i,j;
  for(i = 0; i < len; i++){
    for(j = 0; j < i; j++){
      count = i + j + double2(count);
    }
  }
  return count;
}


int main(){
  unsigned v = exper(7);  //Expected outout: exper(7) = 3860211 = 0x3AE6F3
  // printf("%d\n",v);
}
