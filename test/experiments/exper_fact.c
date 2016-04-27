
//#include <stdio.h>

unsigned fact(n) {
  if(n == 0)
    return 1;
  else
    return fact(n-1)*n;
}


int main(){
  unsigned v = fact(8);  //fact(4) = 24  (0x18)  fact(24)=40320
  //  printf("%d\n",v);
}

