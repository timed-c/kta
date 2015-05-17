

#include <proc/p32mx320f128h.h>

int k = 100;

int foo(int x){
  return x + k;
}

int main(){
  k = 5;
  return foo(4);
}
