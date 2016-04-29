
//#include <stdio.h>

unsigned fact(n) {
  if(n == 0)
    return 1;
  else
    return fact(n-1)*n;
}


unsigned testcomp(unsigned x, unsigned y, unsigned z){
  if(x < 100 || y >= 300 && x < z)
    return x;
  else
    return y;
}


int main(){
  unsigned v = fact(8);  //fact(4) = 24  (0x18)  fact(8)=40320
  //  printf("%d\n",v);
}

