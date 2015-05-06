
#include <stdio.h>
#include "pt.h"

int x,y;

int foo(x){
  int r = 0;
  for(int i=0; i<x; i++)
    r += y;
  return r;
}

void tick(){
  TPP(1);
  if(x){
    y = foo(y);
  }
  TPP(2);
  if(!x){
    y = 20;
  }
  TPP(3);
}


int main(){
  
  y = 2;
  x = 10;
  tick();
  printf("x = %d, y = %d\n", x, y);
  x = 0;
  tick();
  printf("x = %d, y = %d\n", x, y);
}
