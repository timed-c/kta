


#include "tpp.h"

int foo(int x, int y){
  int z = x * y + 3;
  int i;
  for (i=0; i< y; i++) z++;

  TPP(1);

   z++;
h:
  z++;
  if(z < 100){
    z *= y;
    TPP(2);
    z += 777;
    goto h;
  }
  TPP(3);

  return z*2;  
}



int main(){
  return 0;
}

