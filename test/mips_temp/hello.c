

#include <proc/p32mx320f128h.h>

int v;
int k = 100;

int foo(int x){
  return x + k + v;
}

int main(){
  k = 5;
  v = 3;
  return foo(4);
}
